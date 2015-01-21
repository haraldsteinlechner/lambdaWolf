module Primitives where

{-# LANGUAGE OverloadedStrings #-}
import Data.Vec hiding (map)
import Data.Vec.Base hiding (map)
import Data.Vec.LinAlg
import qualified Data.ByteString.Char8 as BS
import GHC.Float
import Data.List
import Control.Monad
import System.Random


type V3  = Vec3 Float
type V4  = Vec4 Float
type M33 = Mat33 Float
type M44 = Mat44 Float

data Box3     = Box3     { min   :: V3, max :: V3 } 
data Geometry = Geometry { dummy :: Int, bb  :: Box3 }
data Trafo3   = Trafo3   { fwd   :: M44, bwd :: M44 }

getRenderJob (Geometry d b) = d
identityTrafo = Trafo3 { fwd = identity, bwd = identity }

multiply (Trafo3 fwd bwd) (Trafo3 fwd' _) = Trafo3 (multmm fwd fwd') bwd

mkRandomGeometry :: IO Geometry
mkRandomGeometry = do
    x:y:z:[] <- replicateM 3 (randomRIO (minValue floatZero,maxValue floatZero))
    x':y':z':[] <- sequence [ randomRIO (x, maxValue floatZero)
                           , randomRIO (y, maxValue floatZero)
                           , randomRIO (z, maxValue floatZero) ]
    id <- randomRIO (0,10000) 
    let randomBox = Box3 (x :. y :. z:. ()) (x' :. y' :. z' :. ())
    return $ Geometry id randomBox

computeCorners :: Box3 -> [V3]
computeCorners (Box3 min max) = [min, maxX :. minY :. minZ :. ()
                                    , minX :. maxY :. minZ :. ()
                                    , maxX :. maxY :. minZ :. ()
                                    , minX :. minY :. maxZ :. ()
                                    , maxX :. minY :. maxZ :. ()
                                    , minX :. maxY :. maxZ :. () ]
    where maxX :. maxY :. maxZ :. () = max
          minX :. minY :. minZ :. () = min

smaller :: Float -> Float -> Float
smaller x y | x <= y    = x
            | otherwise = y 

bigger :: Float -> Float -> Float
bigger x y | x >= y = x
           | otherwise = y

extendBy :: Box3 -> V3 -> Box3
extendBy (Box3 (minX :. minY :. minZ :. ()) (maxX :. maxY :. maxZ :. ())) (x :. y :. z :. ()) =
    let minX' = smaller minX x
        maxX' = bigger  maxX x
        minY' = smaller minY y
        maxY' = bigger  maxY y
        minZ' = smaller minZ z
        maxZ' = bigger  maxZ z
    in Box3 (minX' :. minY' :. minZ' :. ()) (maxX' :. maxY' :. maxZ' :. ())

ofPoints :: [V3] -> Box3
ofPoints = foldl' extendBy nullBox 
  where nullBox = Box3 (maxValue z :. maxValue z :. maxValue z :. ()) (minValue z :. minValue z :. minValue z :. ())
        z = 0 :: Float

transformBB :: Trafo3 -> Box3 -> Box3
transformBB trafo = ofPoints . transformPnts trafo . computeCorners

transformPnts :: Trafo3 -> [V3] -> [V3]
transformPnts (Trafo3 fwd _) = map (project . multmv fwd . homPoint) 

maxValue :: RealFloat a => a -> a
maxValue a = encodeFloat m n where
    b = floatRadix a
    e = floatDigits a
    (_, e') = floatRange a
    m = b ^ e - 1
    n = e' - e

minValue :: RealFloat a => a -> a
minValue a = encodeFloat 1 $ fst (floatRange a) - floatDigits a

floatZero = 0 :: Float

