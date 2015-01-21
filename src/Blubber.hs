module Main where

import Graphics.Formats.Assimp hiding(Clamp)
import Foreign
import Foreign.C.Types
import Foreign.C.String

processing = [  CalcTangentSpace
                , Triangulate
                , JoinIdenticalVertices ]

foreign import ccall unsafe "aiImportFile"
  aiImportFile :: Ptr CChar -> CUInt -> IO (Ptr Int)

main = do 
  x <- getLine 
  body <- newCString "body.obj"
  scene <- aiImportFile body 2
  something <- peekArray 10 scene
  let realScene = castPtr scene :: Ptr Scene
  print something 
  gah <- peek realScene
  return ()
