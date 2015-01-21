{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Arrows #-}
module Reactive (simple,leaveMainLoop,DisplayFunc,reshape,camera) where

import Control.Arrow hiding (right)
import Data.Monoid
import Data.VectorSpace
import GHC.Float
import Control.Monad

import Graphics.UI.GLUT hiding (lookAt, position)
import qualified Graphics.UI.GLFW as GLFW

import FRP.Yampa.GLUT.Adapter
import FRP.Yampa hiding ((^+^),(^-^),right)-- (SF, integral, delay, initially)
import FRP.Yampa.Event

import Debug.Trace
--import System.Posix
--
import qualified Data.Vec as V
import Data.Vec hiding (map,take,translate,identity) --((:.),pack,unpack)


import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.GLU.Raw ( gluLookAt, gluPerspective, gluBuild2DMipmaps )

import Camera
import qualified AbstractCamera as C
import Sg
import RenderTest
import GlHelper
import Assimp2Sg

type DisplayFunc = ((Float, Float), ViewTrafo) -> IO ()

simple :: DisplayFunc -> ViewTrafo -> Reaction
simple displayFunc initialCamera = proc ev -> do
    pos <- ball -< ev
    trafo <- camera initialCamera -< ev
    --fps <- arr (id) <<< derivative <<< time -< ev
    currentTime <- time <<< redisplay -< ev
    lastTime <- iPre 0 <<< time <<< redisplay -< ev
    cycle <- accumHoldBy (\s _ -> s + 1) 0 <<< redisplay -< ev
    
    let fps = 1.0 / (currentTime - lastTime) 
        printFps = maybeToEvent $ Just $ actionIO (when (cycle `mod` 50 == 0) $ print fps)
 
    displayAction <- arr (tagWith (actionIO . displayFunc)) <<< redisplay -< ev
    reshapedAction <- arr (fmap (actionIO . reshape)) <<< reshaped -< ev
    returnA -< mconcat [fmap (\f -> f (pos,trafo) ) displayAction, reshapedAction 
                        ,printFps]

reshape ::  Size -> IO ()
reshape sz@(Size w h) = do
    let b = fromIntegral (w `min` h) * 2
        w' = fromIntegral w / b
        h' = fromIntegral h / b

    viewport $= (Position 0 0, sz)

    matrixMode $= Projection
    loadIdentity
    --frustum (-w') w' (-h') h' 2 100
    gluPerspective 45 (fromIntegral w/fromIntegral h) 1 2000

    matrixMode $= Modelview 0
    loadIdentity

    glEnable gl_TEXTURE_2D
    glShadeModel gl_SMOOTH
    glClearColor 0 0 0 0.5
    glClearDepth 1
    glEnable gl_DEPTH_TEST
    glDepthFunc gl_LEQUAL
    glHint gl_PERSPECTIVE_CORRECTION_HINT gl_NICEST
    lightAmbient  <- newArray' [0.1, 0.1, 0.1, 1.0] 
    lightDiffuse  <- newArray' [1.0, 1.0, 1.0, 1.0]
    lightPosition <- newArray' [1.0, 1.0, 1.0, 1.0]
    glLightfv' gl_LIGHT1 gl_AMBIENT  lightAmbient
    glLightfv' gl_LIGHT1 gl_DIFFUSE  lightDiffuse
    glLightfv' gl_LIGHT1 gl_POSITION lightPosition
    glEnable gl_LIGHT1
    --glDisable gl_TEXTURE_2D
    glEnable gl_LIGHTING
    glEnable gl_TEXTURE_2D
    glEnable gl_CULL_FACE


ball :: SF (Event UI) (Float, Float)
ball = proc ev -> do
         rec
            (Vector2 tx ty) <- simpleMousePosition -< ev
            let mpos = (tx, ty)
                dpos = mpos ^-^ pos
                speed = normalized dpos ^* 0.5
            pos <- integral <<< delay 0.2 zeroV -< speed
         returnA -< pos

camera :: ViewTrafo -> SF (Event UI) ViewTrafo
camera initial = proc ev -> do
                  rec
                    (Vector2 px py) <- simpleMousePosition -< ev
                    pos' <- iPre (0,0)  -< (px,py)
                    leftMouse <- mouseButtonPressed LeftButton -< ev
           
                    let camOrientation = if leftMouse 
                                          then updateViewTrafo oldCam ((px,py) ^-^ pos') 
                                          else oldCam

                    (forward,sidewards) <- camMovement -< ev

                    t1 <- time -< ev
                    t0 <- iPre 0 <<< time -< ev 
                    let speed = 600 * (double2Float t1 - double2Float t0)
                      
                    let camMovement = (moveForward (speed * forward) . 
                                       strafeView (speed * sidewards)) camOrientation

                    --blub <- integral -< (speed * forward)
                    
                    newCamera <- identity -< (updateViewTrafo camMovement (0,0))
                    oldCam <- iPre initial -< newCamera

                  returnA -< newCamera

y :: forall  x.  x  ->  x
y = id 
  where y x = [x | x <- [1,2,3]]

camMovement :: SF (Event UI) (Float,Float)
camMovement = proc ev -> do
               moveForward  <- keyPressed (Left 'w') -< ev
               moveBackward <- keyPressed (Left 's') -< ev
               strafeRight  <- keyPressed (Left 'd') -< ev
               strafeLeft   <- keyPressed (Left 'a') -< ev
               let forward = encodeDir (moveForward,moveBackward)
               let strafe = encodeDir (strafeRight,strafeLeft)
               returnA -< (forward,strafe)                                   

encodeDir :: (Bool,Bool) -> Float
encodeDir (True,False) = 1
encodeDir (False,True) = -1
encodeDir _ = 0


updateViewTrafo :: ViewTrafo -> (Float,Float) -> ViewTrafo
updateViewTrafo trafo (dx,dy) = changeByForward forward' trafo
 where yaw   = V.rotationVec (V.unpack $ up trafo) dx
       pitch = V.rotationVec (V.unpack $ right trafo) (-dy)
       pitchYaw = pitch `V.multmm` yaw
       forwardVec4 = blowUp (forward trafo) 0.0 `V.multvm` pitchYaw 
       forward'    = V.pack $ V.take V.n3 forwardVec4
