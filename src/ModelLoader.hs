module ModelLoader where

import Sg

import Prelude hiding (catch)
import Foreign
import Foreign.Marshal.Alloc
import Graphics.Formats.Assimp hiding(Clamp)
import Graphics.Rendering.OpenGL.GL hiding (Diffuse)
import Data.Array.Storable
import Data.Array.MArray
import Data.Array.IArray hiding (indices)
import qualified Graphics.Rendering.OpenGL.Raw as Raw
import Codec.Image.DevIL
import System.FilePath.Posix
import Graphics.UI.GLUT.Objects
import Control.Exception

import TgaLoader

processing = [  CalcTangentSpace
              , Triangulate
              , JoinIdenticalVertices ]

getEigiRenderAction :: IO (IO ())
getEigiRenderAction = do

  let objs = [  ("body.obj","EigiBodyColor.jpg","EigiBody_NORMAL.jpg")
  --             , ("drool.obj","EigiDrool_COLOR.png","EigiDrool_NORMAL.jpg")
              , ("eyes.obj","EigiEye_COLOR.jpg","EigiEye_NORMAL.jpg")
              , ("lowerTeeth.obj","EigiTeeth_COLOR.jpg","EigiTeeth_NORMAL.jpg")
              , ("upperTeeth.obj","EigiTeeth_COLOR.jpg","EigiTeeth_NORMAL.jpg")
             ] 
  let prefix = "../data/Eigi/"

  renderActions <- mapM (\(obj,color,normal) -> loadObj (prefix++obj) (prefix++color) (prefix++normal)) objs
  return $ sequence_ renderActions

loadObj :: String -> String -> String -> IO (IO ())
loadObj file colormap normalmap = do
  ilInit
  putStrLn $ "importing: " ++ file
  sceneE <- importFile file processing
  let scene = case sceneE of 
                Left errorStr -> error $ "could not load model: " ++ errorStr
                Right s -> s

  putStrLn "imported."

  let sceneMeshes = meshes scene
  let objs = children $ rootNode scene
  let firstLevel = concatMap nodeMeshes objs

  let firstObj = sceneMeshes !! fromIntegral (head firstLevel)

  mesh2VBO firstObj (Just colormap) (Just normalmap)

allGetTextureConf ::  GetTextureConf
allGetTextureConf = GetTextureConf True True True True True True

loadImgToMemory :: String -> IO (Ptr Word8, (Int,Int,Int))
loadImgToMemory p = do
  img <- readImage p
  let ((0,0,0),size@(w,h,bpp)) = bounds img
  mem <- mallocBytes ((w+1)*(h+1)*(bpp+1))
  pokeArray mem (elems img)
  return (mem,size)

loadImageAndGenSetter :: String -> IO (IO ())
loadImageAndGenSetter s = if takeExtension s == ".tga"
                            then loadImageAndGenSetterFast s
                            else loadImageAndGenSetterDevil s 

loadImageAndGenSetterFast :: String -> IO (IO ())
loadImageAndGenSetterFast s = 
  catch (do texture <- loadTgaPtr s createTexture   
            return $ textureBinding Texture2D $= Just texture) nop

nop :: IOException -> IO (IO())
nop _ = return (return ())

createTexture :: Maybe TGA -> Ptr Word8 -> IO TextureObject
createTexture Nothing _ = genAndBindTexture
createTexture (Just tga) ptr = do
  let w' = w tga
  let h' = h tga
  let bpp' = bpp tga
  tex <- genAndBindTexture
  let (format,internal) = if bpp'==24
                           then (BGR,RGBA8)
                           else (BGRA,RGBA8)
  texImage2D Nothing NoProxy 0 internal (TextureSize2D (fromIntegral w') (fromIntegral h')) 0 (PixelData format UnsignedByte ptr) 
  return tex 

{-
loadImageAndGenSetterGlfw :: String -> IO (IO ())
loadImageAndGenSetterGlfw s = do
  tex <- genAndBindTexture
  content <- readFile s
  success <- loadMemoryTexture2D content [NoRescale]
  putStrLn $ "success" ++ show success
  return $ textureBinding Texture2D $= Just tex 
-}

genAndBindTexture :: IO TextureObject
genAndBindTexture = do
  [tex] <- genObjectNames 1
  textureBinding Texture2D $= Just tex
  textureFilter Texture2D $= ((Linear',Nothing),Linear')
  textureWrapMode Texture2D S $= (Repeated,Repeat)
  textureWrapMode Texture2D T $= (Repeated,Repeat)
  return tex

loadImageAndGenSetterDevil :: String -> IO (IO ())
loadImageAndGenSetterDevil p = do
  putStrLn $ "loading image: " ++ p
  (img,(w,h,bpp)) <- loadImgToMemory p 
  putStrLn $ "loaded image: " ++ show (w+1,w+1,bpp+1)
  tex <- genAndBindTexture
  texImage2D Nothing NoProxy 0 RGBA8 (TextureSize2D (fromIntegral w + 1) (fromIntegral h+1)) 0 (PixelData RGBA UnsignedByte img) 
  free img
  return $ textureBinding Texture2D $= Just tex

mesh2VBO :: Mesh -> Maybe String -> Maybe String -> IO (IO ())
mesh2VBO mesh diffuseTexture normalMap = do

  let allIndices = concatMap indices $ faces mesh 

  setColorMap <- case diffuseTexture of 
                  Nothing -> return $ return ()
                  Just p -> loadImageAndGenSetter p 

     
  indexBuffer <- vboOfList ElementArrayBuffer allIndices
  vertexBuffer <- vboOfList ArrayBuffer $ vertices mesh
  normalBuffer <- vboOfList ArrayBuffer $ normals mesh
  coordsBuffer <- vboOfList ArrayBuffer $ textureCoords mesh
  tangentsBuffer <- vboOfList ArrayBuffer $ tangents mesh
  bitangentsBuffer <- vboOfList ArrayBuffer $ bitangents mesh

  let bindAllBuffers = do
      bindBuffer ElementArrayBuffer $= Just indexBuffer
      bindBuffer ArrayBuffer $= Just vertexBuffer
      arrayPointer VertexArray $= VertexArrayDescriptor 3 Float 0 nullPtr 
      bindBuffer ArrayBuffer $= Just normalBuffer
      arrayPointer NormalArray $= VertexArrayDescriptor 3 Float 0 nullPtr
      bindBuffer ArrayBuffer $= Just coordsBuffer
      arrayPointer TextureCoordArray  $= VertexArrayDescriptor 2  Float 12 nullPtr
      clientState VertexArray $= Enabled  
      clientState NormalArray $= Enabled
      clientState TextureCoordArray $= Enabled

  vao <- do
          [vao] <- genObjectNames 1
          bindVertexArrayObject $= Just vao
          bindAllBuffers
          bindVertexArrayObject $= Nothing
          return vao
  -- gl2
  let renderGL2 = do
            bindAllBuffers 
            setColorMap
            drawElements Triangles (fromIntegral $ length allIndices) UnsignedInt nullPtr
  
  let renderGL3 = do
            bindVertexArrayObject $= Just vao
            setColorMap
            drawElements Triangles (fromIntegral $ length allIndices) UnsignedInt nullPtr

  return renderGL3

reportGl :: String -> IO ()
reportGl str = do
  errorCode <- Raw.glGetError
  putStrLn $ "glReport: " ++ str ++ " -> " ++  show errorCode
 
vboOfList :: Storable a => BufferTarget -> [a] -> IO BufferObject
vboOfList arrayType xs =
    let elementSize = sizeOf $ head xs
        size = length xs
        ptrsize = toEnum $ size * elementSize
    in do
       [vboName] <- genObjectNames 1
       bindBuffer arrayType $= Just vboName
       arr <- newListArray (0, size - 1) xs
       withStorableArray arr (\ptr -> bufferData arrayType $= (ptrsize, ptr, StaticDraw))
       bindBuffer arrayType $= Nothing
       --reportErrors
       return vboName

renderQuadCCW :: IO () 
renderQuadCCW =  
 renderPrimitive Quads $ do texCoord (TexCoord2 (0::GLfloat) 0)
                            vertex (Vertex3 (0::GLfloat) 0 0) 
                            texCoord (TexCoord2 (0::GLfloat) 1)
                            vertex (Vertex3 (0::GLfloat) 1 0)
                            texCoord (TexCoord2 (1::GLfloat) 1)
                            vertex (Vertex3 (1::GLfloat) 1 0)
                            texCoord (TexCoord2 (1::GLfloat) 0)
                            vertex (Vertex3 (1::GLfloat) 0 0) 
