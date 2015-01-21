{-# LANGUAGE Arrows #-}
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
import Reactive hiding (simple)


sg = sem_Sg (grid (100,1000) (trace "creating sg" $ lodHierarchy 8 0)) 
             C.Trafo3D { C.forward = V.identity, C.backward = undefined }

sg2 = sem_Sg (grid (10,10) (trace "creating sg" $ eigi)) 
               C.Trafo3D { C.forward = V.identity, C.backward = undefined }

sg3 = sem_Sg (grid (0,0) (trace "creating sg" $ loadSponza)) 
               C.Trafo3D { C.forward = V.identity, C.backward = undefined }


initialCameraTest = lookAt (pack $ 5:.5:.5:.()) (V.pack $ 0:.0:.0:.()) (V.pack $ 0:.1:.0:.())
initialCameraSponza = lookAt (pack $ (-300):.600:.600:.()) (V.pack $ -301:.599:.601:.()) (V.pack $ 0:.1:.0:.())

configuration = [(sg3,initialCameraSponza),(sg,initialCameraTest)]!!0

display :: ViewTrafo -> IO ()
display t = do
    clear [ ColorBuffer, DepthBuffer ]

    --preservingMatrix $ do
    --    translate (Vector3 (realToFrac x) (realToFrac y) (0 :: GLfloat))
    --    renderObject Solid (Teapot 0.1)
    
    loadIdentity
    multMatrix' (C.forward $ trafo t)
    --polygonMode $= (Line,Line)


    --texture Texture2D $= Enabled
    --snd sg t
    --snd (fst configuration) t

    swapBuffers

renderSF :: Sg -> SF (Event UI) (IO ())
renderSF sg = undefined
--multMatrix' (C.forward $ trafo t)


displaySF ::  Sg -> SF (Event UI) (Event Action)
displaySF sg = proc ev -> do
    renderAction <- arr (Event . actionIO . renderSetup) <<< renderSF sg -< ev
    returnA -< renderAction 
  where renderSetup a = loadIdentity >> a >> swapBuffers

simple :: ViewTrafo -> Reaction
simple initialCamera = proc ev -> do
    trafo <- camera initialCamera -< ev
    currentTime <- time <<< redisplay -< ev
    lastTime <- iPre 0 <<< time <<< redisplay -< ev
    cycle <- accumHoldBy (\s _ -> s + 1) 0 <<< redisplay -< ev
    
    let fps = 1.0 / (currentTime - lastTime) 
        printFps = maybeToEvent $ Just $ actionIO (when (cycle `mod` 50 == 0) $ print fps)
 
    displayAction <- arr (tagWith (actionIO . display)) <<< redisplay -< ev
    reshapedAction <- arr (fmap (actionIO . reshape)) <<< reshaped -< ev
    displayAction' <- displaySF loadSponza -< ev
    returnA -< mconcat [reshapedAction, --fmap (\f -> f trafo) displayAction, reshapedAction 
                        displayAction', printFps]


main = do
  simpleInit "LambdaWolf"
  adapt leaveMainLoop (simple $ snd configuration)
