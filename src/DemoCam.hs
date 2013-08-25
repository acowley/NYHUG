{-# LANGUAGE DataKinds, TypeOperators, TupleSections #-}
import Control.Applicative
import Control.Parallel.CLUtil
import Control.Parallel.CLUtil.Monad
import CLGLInterop
import qualified Data.Vector.Storable as V
import Data.Vinyl
import Foreign.Ptr (nullPtr)
import Graphics.GLUtil
import Graphics.GLUtil.Camera3D
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW (Key(KeyEsc))
import Linear
import OpenCV.HighCV
import OpenCV.PixelUtils (packPixels)
import Keyboard3D (moveCamera)
import Graphics.VinylGL
import Window (initGL, UI(..))

type AppInfo = PlainRec '[ "proj"  ::: M44 GLfloat ]
type Pos = "vertexPos" ::: V4 GLfloat

pos :: Pos
pos = Field

-- | Uniform grid of vertices with @n*2@ vertices on a side.
grid :: Integral a => a -> [V2 GLfloat]
grid n = map (fmap ((*s) . fromIntegral)) [V2 x y | y <- [-n..n], x <- [-n..n]]
  where s = 1 / fromIntegral n

-- | Indices of an @n@ x @n@ grid in row-major order.
inds :: Word32 -> [Word32]
inds n = take (numQuads*6) $ concat $ iterate (map (n+)) row
  where numQuads = fromIntegral $ (n - 1) * (n - 1)
        row = concatMap (\c -> map (c+) [0,n,1,1,n,n+1]) [0..n - 2]

ripple :: BufferObject -> CL (Vector Word8 -> CL (), IO ())
ripple b = do verts <- bufferFromGL b :: CL (CLBuffer (V4 Float))
              let workSize = Work1D $ bufferLength verts
                  sz = [640,480] :: [Int]
              img <- allocImage [CL_MEM_READ_ONLY] sz :: CL (CLImage1 Float)
              img' <- allocImage [CL_MEM_READ_WRITE] sz :: CL (CLImage1 Float)
              let cleanup = () <$ (releaseObject verts >> releaseObject img)
              k <- getKernel "etc/ripples.cl" "ripple"
              blur <- mkBlur
              return . (,cleanup) $ \e -> 
                do writeImage img (V.map ((/255) . fromIntegral) e)
                   blur img img'
                   withGLObjects [bufferObject verts] $
                     runKernelCL k img' verts workSize

mkBlur :: CL (CLImage n a -> CLImage n a -> CL ())
mkBlur = do k <- getKernel "etc/ripples.cl" "localMax"
            return $ \i b -> do () <- runKernelCL k i b stepX n w
                                runKernelCL k i b stepY n w
  where stepX = V2 1 0 :: V2 CInt
        stepY = V2 0 1 :: V2 CInt
        w = Work2D 640 480
        n = 4 :: CInt
        -- mask :: V.Vector CFloat
        -- mask = let h = [0.05,0.09,0.12,0.15]
        --        in V.fromList $ h ++ 0.16 : reverse h 

-- | Refresh a 'TextureObject' with images from a camera, and dump an
-- edge image into the given function.
textureCam :: (Vector Word8 -> IO ()) -> IO (IO TextureObject)
textureCam writeEdges = 
  do [vid] <- genObjectNames 1
     textureBinding Texture2D $= Just vid
     texImage2D Nothing NoProxy 0 RGBA' (TextureSize2D 640 480) 0
                (PixelData RGB UnsignedByte nullPtr)
     textureFilter Texture2D $= ((Linear', Nothing), Linear')
     texture2DWrap $= (Repeated, Repeat)
     vidCam <- createCameraCapture (Just 0)
     let info = TexInfo 640 480 TexBGR
         refresh = do img <- vidCam :: IO ColorImage
                      let img' = canny 70 110 3 . convertBGRToGray $ img
                      withImagePixels img' writeEdges
                      reloadTexture vid . info $ packPixels img
     return (vid <$ refresh)

-- | Prepare the rendering function and a cleanup action.
setup :: IO (AppInfo -> IO (), IO ())
setup = do clearColor $= Color4 0.3 0.6 0.3 1
           depthFunc $= Just Lequal
           vb <- bufferVertices . map ((pos =:) . aux) $ grid halfSize
           eb <- bufferIndices $ inds fullSize
           gpu <- initFromGL CL_DEVICE_TYPE_GPU
           (updateEdges,cleanup) <- runCL gpu . ripple $ getVertexBuffer vb
           refresh <- textureCam $ runCL gpu . updateEdges
           s <- loadShaderProgram ("etc/ripples.vert") ("etc/ripples.frag")
           vao <- makeVAO $
                  do currentProgram $= Just (program s)
                     enableVertices' s vb
                     bindVertices vb
                     bindBuffer ElementArrayBuffer $= Just eb
                     setUniforms s (tex =: 0)
           let ss = setUniforms s
           return . (, cleanup >> ezRelease gpu) $ \appInfo -> withVAO vao $
             do ss appInfo
                t' <- refresh
                withTextures2D [t'] $ drawIndexedTris (8*halfSize*halfSize)
  where aux (V2 x y) = V4 x 0 y 1
        tex :: "tex" ::: GLint
        tex = Field
        halfSize = 50
        fullSize = fromIntegral $ halfSize * 2 + 1

loop :: IO UI -> IO ()
loop tick = 
  do (draw,cleanup) <- setup
     let go c = do ui <- tick
                   clear [ColorBuffer, DepthBuffer]
                   let V2 ww wh = fromIntegral <$> windowSize ui
                       mProj = projectionMatrix (deg2rad 45) (ww / wh) 0.01 100
                       info = Field =: (mProj !*! camMatrix c)
                   draw info
                   if keysPressed ui ^. contains KeyEsc
                   then cleanup
                   else go (moveCamera ui c)
     go cam0
  where cam0 = tilt (-35) $ dolly (V3 0 2 3) fpsCamera

main :: IO ()
main = usage >> initGL "NY Haskellers" 640 480 >>= loop

usage :: IO ()
usage = putStrLn "Arrow keys to translate, shift+arrow to rotate, esc to exit!"
