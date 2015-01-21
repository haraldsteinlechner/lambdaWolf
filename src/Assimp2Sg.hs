module Assimp2Sg where

import Graphics.Formats.Assimp
import System.FilePath.Posix
import Text.Printf
import qualified Data.Map as M
import Data.Map ()
import Foreign.C.Types
import Control.Monad
import System.IO.Unsafe
import Data.Maybe
import Data.String.Utils
import Codec.Image.DevIL
import Data.Time.Clock
import Control.DeepSeq

import Data.Vect.Float
import Sg hiding (Node)
import qualified Sg as S
import ModelLoader
import RenderTest

instance NFData Sg where
  rnf a = a `seq` ()

processing'  = [ CalcTangentSpace
               , Triangulate
               , JoinIdenticalVertices ]

type MaterialAction = IO ()
type MeshWithAction = (IO (),Mesh)
type MaterialCache = M.Map CUInt MaterialAction

loadSponzaIO = loadSg "sponza.obj" "../data/sponza/"

loadSponza = unsafePerformIO sgIO
  where sgIO = loadSg "sponza.obj" "../data/sponza/"

loadSg :: String -> FilePath -> IO Sg
loadSg modelName baseDir = do
  ilInit
  sceneE <- importFile (joinPath [baseDir,modelName]) processing'
  case sceneE of 
    Right scene -> loadScene baseDir scene
    Left errorS -> error $ "loadSg failed: " ++ errorS

loadScene :: FilePath -> Scene -> IO Sg
loadScene baseDir scene = do
  start <- getCurrentTime
  materialActions <- mapM (loadMaterial baseDir) $ materials scene
  meshActions <- mapM (loadMesh baseDir) $ meshes scene 
  let result = force $ buildSg (zip meshActions (meshes scene)) materialActions scene
  end <- getCurrentTime
  printf "loaded scene in: %s seconds.\n" (show $ diffUTCTime end start)
  return result

buildSg :: [MeshWithAction] -> [MaterialAction] -> Scene -> Sg
buildSg renderActions matActions scene = buildSg' renderActions matActions $ rootNode scene

buildSg' :: [MeshWithAction] -> [MaterialAction] -> Node -> Sg
buildSg' xs ys node = S.Node (sgs node) (children' node)
  where children' = toGroup . map ( buildSg' xs ys ) . children 
        getRenderCall meshIndex = let (action,mesh) = xs!!meshIndex
                                  in ys!!(fromIntegral $ materialIndex mesh)>>action
        sgs = toGroup  . map ( Leaf . renderObj . getRenderCall . fromIntegral ) . nodeMeshes


toGroup :: [Sg] -> Sg
toGroup = foldr S.Node S.Nil  

noneGetTextureConf = GetTextureConf False False False False False False

loadMaterial :: FilePath -> Material -> IO MaterialAction
loadMaterial baseDir material = do
  let diffuseTextureM = getMaterialData material Diffuse
  case diffuseTextureM of 
    Just diffuseTexture -> do putStrLn $ "mat: " ++ show diffuseTexture
                              let fixedTexture = replace "\\" [pathSeparator] diffuseTexture 
                              let fullPath = joinPath [baseDir, fixedTexture]
                              putStrLn $ "full path: " ++ fullPath
                              loadImageAndGenSetter (joinPath [baseDir, fixedTexture])
    Nothing -> do putStrLn "warning: texture not found in material"
                  return (return ())

getMaterialData :: Material -> TextureType -> Maybe String
getMaterialData mat semantic' = listToMaybe props
  where props = [fromJust s | p <- properties mat, 
                              semantic p == semantic', 
                              let s = getTexString (mData p), isJust s]

getTexString :: MaterialData -> Maybe String
getTexString (MaterialString s) = Just s 
getTexString _ = Nothing

loadMesh :: FilePath -> Mesh -> IO (IO ())
loadMesh baseDir mesh = do
  let material = materialIndex mesh
  mesh2VBO mesh Nothing Nothing

getBB :: Mesh -> String
getBB mesh = let minCorner = foldl1 min' vs
                 maxCorner = foldl1 max' vs
             in show minCorner  ++ "," ++ show maxCorner
  where vs = vertices mesh

max' = select max
min' = select min

select :: (Float -> Float -> Float) -> Vec3 -> Vec3 -> Vec3
select f (Vec3 x y z) (Vec3 x' y' z') = Vec3 (f x x') (f y y') (f z z')
