module AbstractCamera (Trafo(..)) where

import Data.Vec

class Camera a where
  forwardVec :: a -> Vec3F
  rightVec   :: a -> Vec3F
  upVec      :: a -> Vec3F
  
  trafo    :: a -> Trafo
  position :: a -> Vec3F

  moveForward :: a -> a
  strafeRight :: a -> a
  moveUp      :: a -> a
  
  rpy :: Float -> Float -> Float -> a -> a


data Trafo = Trafo3D { forward :: Mat44 Float, backward :: Mat44 Float }
