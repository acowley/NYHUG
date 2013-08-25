{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables #-}
module CLGLInterop (initFromGL, bufferFromGL, imageFromGL2D,
                    withGLObjects) where
import Control.Parallel.CLUtil
import Control.Parallel.CLUtil.Monad
import Control.Parallel.CLUtil.Monad.CL
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (nullPtr, Ptr)
import Foreign.Storable (Storable(peek, sizeOf))
import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.Raw.Core31

foreign import ccall "CGLGetCurrentContext"
  cGLGetCurrentContext :: IO (Ptr ())

foreign import ccall "CGLGetShareGroup"
  cGLGetShareGroup :: Ptr () -> IO (Ptr ())

initFromGL :: CLDeviceType -> IO OpenCLState
initFromGL devType = 
  do dev:_ <- clGetDeviceIDs nullPtr devType
     shareGroup <- cGLGetCurrentContext >>= cGLGetShareGroup
     context <- clCreateContext [CL_CGL_SHAREGROUP_KHR shareGroup]
                                [dev]
                                putStrLn
     q <- clCreateCommandQueue context dev []
     return $ OpenCLState dev context q

imageFromGL2D :: TextureObject -> CL (CLImage n a)
imageFromGL2D tex@(TextureObject t) =
  do c <- clContext `fmap` ask
     liftIO $ 
       do textureBinding Texture2D $= Just tex
          TextureSize2D w h <- get $ textureSize2D (Left Texture2D) 0
          textureBinding Texture2D $= Nothing
          CLImage (fromIntegral w, fromIntegral h, 1) `fmap`
            clCreateFromGLTexture2D c [CL_MEM_READ_WRITE] 
                                    gl_TEXTURE_2D (0::CInt) t

bufferFromGL :: forall a. Storable a => BufferObject -> CL (CLBuffer a)
bufferFromGL bo@(BufferObject b) = 
  do c <- clContext `fmap` ask
     liftIO $
       do bindBuffer ArrayBuffer $= Just bo
          n <- alloca $ \ptr ->
            do glGetBufferParameteriv gl_ARRAY_BUFFER gl_BUFFER_SIZE ptr
               peek ptr :: IO GLint
          CLBuffer (fromIntegral n `quot` sizeOf (undefined::a)) `fmap`
            clCreateFromGLBuffer c [CL_MEM_READ_WRITE] b

withGLObjects :: [CLMem] -> CL r -> CL r
withGLObjects obs m = do q <- clQueue `fmap` ask
                         liftIO $ do ev <- clEnqueueAcquireGLObjects q obs []
                                     clWaitForEvents [ev]
                                     clReleaseEvent ev
                         r <- m
                         liftIO $ do ev <- clEnqueueReleaseGLObjects q obs []
                                     clWaitForEvents [ev]
                                     clReleaseEvent ev
                         return r
