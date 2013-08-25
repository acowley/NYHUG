{-# LANGUAGE DataKinds, TypeOperators, TupleSections #-}
import Control.Applicative
import Control.Parallel.CLUtil
import Control.Parallel.CLUtil.Monad
import Control.Parallel.CLUtil.Monad.CL
import CLGLInterop
import Data.Foldable (foldMap)
import Data.Vinyl
import Foreign.Ptr (nullPtr)
import Graphics.GLUtil
import Graphics.GLUtil.Camera3D
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW (Key(KeyEsc))
import Linear
import OpenCV.HighCV
import OpenCV.Filtering
import OpenCV.PixelUtils (packPixels)
import System.FilePath ((</>))
import Keyboard3D (moveCamera)
import Graphics.VinylGL
import Window (initGL, UI(..))

import qualified Data.Vector.Storable as V

type AppInfo = PlainRec '[ "proj"  ::: M44 GLfloat ]
type Pos = "vertexPos" ::: V3 GLfloat

pos :: Pos
pos = Field

-- Uniform grid of vertices with n*2 vertices on a side
grid :: Int -> [V2 GLfloat]
grid n = map (fmap ((*s) . fromIntegral)) [V2 x y | y <- [-n..n], x <- [-n..n]]
  where s = 1 / fromIntegral n

-- Indices of an nxn grid in row-major order.
inds :: Word32 -> [Word32]
inds n = take (numQuads*6) $ concat $ iterate (map (n+)) row
  where numQuads = fromIntegral $ (n - 1) * (n - 1)
        row = foldMap (\c -> map (c+) [0,n,1,1,n,n+1]) [0..n - 2]

ripple :: BufferObject -> CL (Vector Word8 -> CL ())
ripple b = do verts <- bufferFromGL b :: CL (CLBuffer (V3 Float))
              let workSize = Work1D $ bufferLength verts
                  --imgSz = [160,120] :: [Int]
                  imgSz = [640,480] :: [Int]
              img <- allocImage [CL_MEM_READ_ONLY] imgSz :: CL (CLImage1 Float)
              k <- getKernel "etc/ripples.cl" "ripple"
              return $ \e -> do writeImage img (V.map ((/255) . fromIntegral) e)
                                withGLObjects [bufferObject verts] $
                                  runKernelCL k img verts workSize

withGLObjects :: [CLMem] -> CL r -> CL r
withGLObjects obs m = do q <- clQueue <$> ask
                         liftIO $ do ev <- clEnqueueAcquireGLObjects q obs []
                                     clWaitForEvents [ev]
                                     clReleaseEvent ev
                         r <- m
                         liftIO $ do ev <- clEnqueueReleaseGLObjects q obs []
                                     clWaitForEvents [ev]
                                     clReleaseEvent ev
                         return r
                       

textureCam :: (Vector Word8 -> IO ()) -> IO (IO TextureObject)
textureCam f = 
  do [vid] <- genObjectNames 1
     textureBinding Texture2D $= Just vid
     texImage2D Nothing NoProxy 0 RGBA' (TextureSize2D 640 480) 0
                (PixelData RGB UnsignedByte nullPtr)
     textureFilter Texture2D $= ((Linear', Nothing), Linear')
     texture2DWrap $= (Repeated, Repeat)
     vidCam <- createCameraCapture (Just 0)
     let info = TexInfo 640 480 TexBGR
         refresh = do img <- vidCam :: IO ColorImage
                      let img' = smoothGaussian 7 . canny 70 110 3 . convertBGRToGray $ img
                     -- . resize CV_INTER_CUBIC 160 120 
                      withImagePixels img' f
                      --reloadTexture vid . info $ packPixels img
                      withImagePixels img' $ reloadTexture vid . TexInfo 640 480 TexMono
     return (vid <$ refresh)

setup :: IO (AppInfo -> IO (), IO ())
setup = do clearColor $= Color4 0.3 0.6 0.3 1
           depthFunc $= Just Lequal
           vb <- bufferVertices . map ((pos =:) . aux) $ grid halfSize
           eb <- bufferIndices $ inds fullSize
           gpu <- initFromGL CL_DEVICE_TYPE_GPU
           updateEdges <- runCL gpu . ripple $ getVertexBuffer vb
           refresh <- textureCam $ runCL gpu . updateEdges
           s <- loadShaderProgram ("etc"</>"ripples.vert") ("etc"</>"ripples.frag")
           vao <- makeVAO $
                  do currentProgram $= Just (program s)
                     enableVertices' s vb
                     bindVertices vb
                     bindBuffer ElementArrayBuffer $= Just eb
                     setUniforms s (tex =: 0)
           let ss = setUniforms s
           return . (, ezRelease gpu) $ \appInfo -> withVAO vao $
             do ss appInfo
                t' <- refresh
                withTextures2D [t'] $ drawIndexedTris (2*fullSize*fullSize)
  where aux (V2 x y) = V3 x 0 y
        tex :: "tex" ::: GLint
        tex = Field
        halfSize = 50
        fullSize :: Integral a => a
        fullSize = fromIntegral $ halfSize * 2 + 1

loop :: IO UI -> IO ()
loop tick = 
  do (draw,cleanup) <- setup
     let go c = do ui <- tick
                   clear [ColorBuffer, DepthBuffer]
                   let V2 ww wh = fromIntegral <$> (windowSize ui - V2 160 120)
                       mProj = projectionMatrix (deg2rad 30) (ww / wh) 0.01 100
                       info = Field =: (mProj !*! camMatrix c)
                   draw info
                   if keysPressed ui ^. contains KeyEsc
                   then cleanup
                   else go (moveCamera ui c)
     go cam0
  where cam0 = tilt (-35) $ dolly (V3 0 2 3) fpsCamera

main :: IO ()
main = usage >> initGL "Camorama" 640 480 >>= loop

usage :: IO ()
usage = putStrLn "Arrow keys to translate, shift+arrow to rotate, esc to exit!"
