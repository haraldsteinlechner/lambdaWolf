module GlHelper where

import Control.Arrow hiding (right)
import Data.Monoid
import Data.VectorSpace

import Graphics.UI.GLUT hiding (lookAt, position,GLenum,GLfloat)

import FRP.Yampa.GLUT.Adapter
import FRP.Yampa hiding ((^+^),(^-^),right)-- (SF, integral, delay, initially)
import FRP.Yampa.Event
import FRP.Yampa.GLUT.UI

import Debug.Trace
--import System.Posix
--
import qualified Data.Vec as V
import Data.Vec hiding (map,take,translate,identity) --((:.),pack,unpack)

import Data.IORef
import Foreign.Marshal.Array ( newArray, allocaArray, peekArray, pokeArray )
import Graphics.Rendering.OpenGL.Raw
--import Graphics.Rendering.OpenGL
import Graphics.Rendering.GLU.Raw ( gluLookAt, gluPerspective, gluBuild2DMipmaps )
import Foreign.Storable ( Storable )
import Foreign ( withForeignPtr, plusPtr
               , ForeignPtr, newForeignPtr_ )

newArray' :: Storable a => [a] -> IO (ForeignPtr a)
newArray' xs = newArray xs >>= newForeignPtr_

glLightfv' :: GLenum -> GLenum -> ForeignPtr GLfloat -> IO ()
glLightfv' l a fp =
  withForeignPtr fp $ glLightfv l a
