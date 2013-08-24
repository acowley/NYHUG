{-# LANGUAGE DataKinds, TypeOperators #-}
import Control.Applicative
import Control.Parallel.CLUtil
import Control.Parallel.CLUtil.Monad
import CLGLInterop
import Data.Vinyl
import Foreign.Ptr (nullPtr)
import Graphics.GLUtil
import Graphics.GLUtil.Camera3D
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW (Key(KeyEsc))
import Linear
import OpenCV.HighCV
import OpenCV.PixelUtils (packPixels)
import System.FilePath ((</>))
import Keyboard3D (moveCamera)
import Graphics.VinylGL
import Window (initGL, UI(..))

type AppInfo = PlainRec '[ "proj"  ::: M44 GLfloat ]
type Pos = "vertexPos" ::: V3 GLfloat

pos :: Pos
pos = Field

textureCam :: IO (TextureObject, IO ())
textureCam = do [t] <- genObjectNames 1
                textureBinding Texture2D $= Just t
                texImage2D Nothing NoProxy 0 RGBA' (TextureSize2D 640 480) 0
                           (PixelData RGB UnsignedByte nullPtr)
                vidCam <- createCameraCapture (Just 0)
                let info = TexInfo 640 480 TexMono -- TexBGR
                    refresh = do img <- vidCam :: IO ColorImage
                                 let edges = canny 70 110 3 . convertBGRToGray $ img
                                 withImagePixels edges $ reloadTexture t . info
                                 -- reloadTexture t . info $ edges -- packPixels img
                                 -- withImagePixels img $ reloadTexture t . info
                return (t, refresh)

setup :: IO (AppInfo -> IO ())
setup = do clearColor $= Color4 0.3 0.6 0.3 1
           depthFunc $= Just Lequal
           (t,refresh) <- textureCam
           vb <- bufferVertices . map ((pos =:) . aux) $ V2 <$> [-1,1] <*> [-1,1]
           s <- loadShaderProgram ("etc"</>"ripples.vert") ("etc"</>"ripples.frag")
           vao <- makeVAO $
                  do currentProgram $= Just (program s)
                     enableVertices' s vb
                     bindVertices vb
                     setUniforms s (tex =: 0)
                     textureBinding Texture2D $= Just t
                     textureFilter Texture2D $= ((Linear', Nothing), Linear')
                     texture2DWrap $= (Repeated, Repeat)
           let ss = setUniforms s
           return $ \appInfo -> withVAO vao $
             do refresh
                ss appInfo
                withTextures2D [t] $ drawArrays TriangleStrip 0 4
  where aux (V2 x y) = V3 x 0 y
        tex :: "tex" ::: GLint
        tex = Field

loop :: IO UI -> IO ()
loop tick = setup >>= go cam0
  where go :: Camera GLfloat -> (AppInfo -> IO ()) -> IO ()
        go c draw = 
          do ui <- tick
             clear [ColorBuffer, DepthBuffer]
             let V2 ww wh = fromIntegral <$> (windowSize ui - V2 160 120)
                 mProj = projectionMatrix (deg2rad 30) (ww / wh) 0.01 100
                 info = Field =: (mProj !*! camMatrix c)
             draw info
             if keysPressed ui ^. contains KeyEsc
             then return () -- terminate
             else go (moveCamera ui c) draw
        cam0 = tilt (-35) $ dolly (V3 0 2 3) fpsCamera

main :: IO ()
main = usage >> initGL "Camorama" 640 480 >>= loop

usage :: IO ()
usage = putStrLn "Arrow keys to translate, shift+arrow to rotate, esc to exit!"
