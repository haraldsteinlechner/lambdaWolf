module RenderTest where

import Sg

import Control.Monad
import qualified AbstractCamera as C
import Camera
import qualified Data.Vec as V
import Graphics.UI.GLUT.Objects
import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.OpenGL.GL.DisplayLists
import Foreign.Marshal.Array ( newArray, allocaArray, peekArray, pokeArray )
import GHC.Float
import Foreign.Ptr
import System.IO.Unsafe
import Debug.Trace
import ModelLoader

shift :: V.Vec3F -> C.Trafo -> C.Trafo
shift vec old = old { C.forward = (C.forward old) `V.multmm` t }
 where t = V.translation $ V.unpack vec

multMatrix' mat = allocaArray (16 * 4)
                          (\ptr -> do pokeArray ptr (map convertGlNumbers (V.matToList $ V.transpose mat))
                                      glMultMatrixf ptr)

convertGlNumbers :: Float -> GLfloat
convertGlNumbers = fromRational . toRational 

applyTrafo :: C.Trafo -> IO ()
applyTrafo t = multMatrix' (C.forward t)

{-# NOINLINE solidCube #-} 
solidCube :: IO ()
solidCube = unsafePerformIO $ do list <- defineNewList Compile (renderObject Solid (Cube 1.0))
                                 return $ callList list

renderObj :: IO () -> RenderAction
renderObj obj trafo viewTrafo = do-- glPushMatrix
                                  -- applyTrafo trafo
                                   obj
                                  -- glPopMatrix

nop :: RenderAction
nop _ _ = return ()

sampleSg :: Sg
sampleSg = Trafo left (shift $ V.Vec3F 1 0 0)
 where left = Trafo geometry (shift $ V.Vec3F 0 0 1)
       geometry = Leaf $ renderObj (renderObject Solid (Cube 1.0))


--lodSg :: Int -> Float -> Sg
lodSg ::  Sg -> Sg -> Float -> Sg
lodSg low high threshhold = Lod low high lodDecider
 where lodDecider camera trafo (min,max) = (V.dot distVec distVec) < threshhold
                   where camLoc = V.unpack $ position camera
                         distVec = camLoc - min

grid ::  (Integral a) => (a, a) -> Sg -> Sg
grid (sx,sy) obj = Prelude.foldr (\pos a -> Node a $ createNode pos) Nil positions
  where positions = [(x,y) | x <- [0..sx], y <- [0..sy]]
        createNode (x,y) = let fx = fromIntegral x
                               fy = fromIntegral y
                           in Trafo obj (shift $ V.Vec3F fx 0 fy)

nopSg :: Sg
nopSg = Nil

cubeMengerSg ::  Float -> Sg
cubeMengerSg = lodSg cubeSg (mengerSg 5)

cubeSg ::  Sg
cubeSg = Leaf $ renderObj solidCube

mengerSg ::  NumLevels -> Sg
mengerSg quality = Leaf $ renderObj (cache!!(fromIntegral quality))
  where cache = [renderCall | i <- [0..], renderCall <- renderCall i]
        renderCall quality = let qi = fromIntegral quality
                                 list = unsafePerformIO $
                                             defineNewList Compile (renderObject Solid (SierpinskiSponge qi))
                             in return $ callList list


lodHierarchy ::  NumLevels -> NumLevels -> Sg
lodHierarchy maxLevels levels | levels >= maxLevels = Nil
                              | levels == 0 = lodSg nopSg next 10000
                              | otherwise = lodSg low next 
                                               (1.5 * (fromIntegral $ maxLevels - levels))
  where low  = mengerSg levels
        next = lodHierarchy maxLevels (levels+1) 


eigi :: Sg
eigi = Leaf $ renderObj action
  where action = unsafePerformIO getEigiRenderAction
