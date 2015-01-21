{-# LANGUAGE TypeSynonymInstances #-}
--
-- This code was created by Jeff Molofee '99 (ported to Haskell GHC 2005)
--

module Main where

import qualified Graphics.UI.GLFW as GLFW
-- everything from here starts with gl or GL
import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.GLU.Raw ( gluLookAt, gluPerspective, gluBuild2DMipmaps )
import Data.Bits ( (.|.) )
import System.Exit ( exitWith, ExitCode(..) )
import Control.Monad ( forever )
import Foreign ( withForeignPtr, plusPtr
               , ForeignPtr, newForeignPtr_ )
import Foreign.Storable ( Storable )
import Foreign.Marshal.Array ( newArray, allocaArray, peekArray, pokeArray )
import qualified Data.ByteString.Internal as BSI
import Util ( Image(..), bitmapLoad )
--import Paths_nehe_tuts

import Data.IORef
import Data.Vec hiding(map)
import Data.Vec.Base hiding(map)
import Data.Vec.LinAlg
import Data.Vec.Packed
import Foreign.Ptr
import GHC.Float
import Camera
import qualified AbstractCamera as C
import qualified Data.Vec as V
import Graphics.UI.GLUT.Objects
import Graphics.UI.GLUT.Initialization
import System.Clock

import Sg
import RenderTest
import Assimp2Sg

newArray' :: Storable a => [a] -> IO (ForeignPtr a)
newArray' xs = (newArray xs) >>= newForeignPtr_

glLightfv' :: GLenum -> GLenum -> ForeignPtr GLfloat -> IO ()
glLightfv' l a fp =
  withForeignPtr fp $ glLightfv l a

initGL :: IO [GLuint]
initGL = do
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
  --glBlendFunc gl_SRC_ALPHA gl_ONE
  loadGLTextures

getDataFileName = return . id

loadGLTextures :: IO [GLuint]
loadGLTextures = do
  fp <- getDataFileName "../data/TestData/glass.bmp"
  Just (Image w h pd) <- bitmapLoad fp
  let numTextures = 3
  texs <- allocaArray numTextures $ \p -> do
            glGenTextures (fromIntegral numTextures) p
            peekArray numTextures p
  let (ptr, off, _) = BSI.toForeignPtr pd
  _ <- withForeignPtr ptr $ \p -> do
    let p' = p `plusPtr` off
        glNearest = fromIntegral gl_NEAREST
        glLinear  = fromIntegral gl_LINEAR
    -- create nearest filtered texture
    glBindTexture gl_TEXTURE_2D (texs!!0)
    glTexImage2D gl_TEXTURE_2D 0 3
      (fromIntegral w) (fromIntegral h)
      0 gl_RGB gl_UNSIGNED_BYTE p'
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER glNearest
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER glNearest
    -- create linear filtered texture
    glBindTexture gl_TEXTURE_2D (texs!!1)
    glTexImage2D gl_TEXTURE_2D 0 3
      (fromIntegral w) (fromIntegral h)
      0 gl_RGB gl_UNSIGNED_BYTE p'
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER glLinear
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER glLinear
    -- create mipmap filtered texture
    glBindTexture gl_TEXTURE_2D (texs!!2)
    glTexImage2D gl_TEXTURE_2D 0 3
      (fromIntegral w) (fromIntegral h)
      0 gl_RGB gl_UNSIGNED_BYTE p'
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER glLinear 
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER
      (fromIntegral gl_LINEAR_MIPMAP_NEAREST)
    gluBuild2DMipmaps gl_TEXTURE_2D 3 (fromIntegral w)
      (fromIntegral h) gl_RGB gl_UNSIGNED_BYTE p'
  return texs

resizeScene :: GLFW.WindowSizeCallback
resizeScene w     0      = resizeScene w 1 -- prevent divide by zero
resizeScene width height = do
  glViewport 0 0 (fromIntegral width) (fromIntegral height)
  glMatrixMode gl_PROJECTION
  glLoadIdentity
  gluPerspective 45 (fromIntegral width/fromIntegral height) 1 5000
  glMatrixMode gl_MODELVIEW
  glLoadIdentity
  glFlush

matDouble2Float :: Mat44 Double -> Mat44 Float
matDouble2Float = matFromLists . (map (map double2Float) ) . matToLists 

clock :: Clock
clock = Realtime


--sg = sem_Sg (grid (40,40) (cubeMengerSg 10.0)) (C.Trafo3D { C.forward = identity, C.backward = undefined })
--sg = sem_Sg (grid (800,800) $ Node nopSg nopSg) (C.Trafo3D { C.forward = identity, C.backward = undefined })

--sg = sem_Sg (grid (100,1000) (lodHierarchy 8 0)) (C.Trafo3D { C.forward = identity, C.backward = undefined })

sg = sem_Sg (grid (0,0) loadSponza) (C.Trafo3D { C.forward = identity, C.backward = undefined })

data ControllerState = ControllerState { mouseDown :: Bool,
                            mousePos :: Packed (Vec2 Double), 
                            lastPos :: Packed (Vec2 Double),
                            rotMat :: Mat44D,
                            zoomIn :: Bool,
                            zoomOut :: Bool,
                            viewTrafo :: ViewTrafo,
                            strafe :: Int, cycleCount :: Int }

drawScene :: [GLuint] -> IORef GLfloat -> IORef GLfloat
          -> IORef GLfloat -> IORef GLfloat -> IORef GLfloat 
          -> IORef Int
          -> IORef ControllerState -> IO ()
drawScene texs xrot yrot xspeed yspeed zdepth filt controllerState  = do

  start <- getTime clock

  -- clear the screen and the depth buffer
  glClear $ fromIntegral  $  gl_COLOR_BUFFER_BIT
                         .|. gl_DEPTH_BUFFER_BIT
  glLoadIdentity -- reset view

  glTranslatef 0 0 (-5.0) --Move left 5 Units into the screen

  zd <- readIORef zdepth
  glTranslatef 0 0 zd --Move left 5 Units into the screen
{-
  xr <- readIORef xrot
  yr <- readIORef yrot
  glRotatef xr 1 0 0 -- Rotate the triangle on the Y axis
  glRotatef yr 0 1 0 -- Rotate the triangle on the Y axis
-}
  let angleToRad = (/57.3)

  xr <- readIORef xrot
  yr <- readIORef yrot


  let convertGlNumbers = fromRational . toRational  

  let multMatrix mat = allocaArray (16 * 4)
                          (\ptr -> do pokeArray ptr $ map convertGlNumbers (matToList mat)
                                      glMultMatrixd ptr)

 
  let multMatrix' mat = allocaArray (16 * 4)
                          (\ptr -> do pokeArray ptr $ map (convertGlNumbers . float2Double) (matToList mat)
                                      glMultMatrixd $ castPtr ptr)

                  
--  multMatrix $ rotationX (convertGlNumbers $ angleToRad xr)
 -- multMatrix $ rotationY (convertGlNumbers $ angleToRad yr)

  state <- readIORef controllerState

  let toAngle x = (-x) * 0.01
  let egoController = True

  let  pos = if mouseDown state then mousePos state else lastPos state
       last = lastPos state
       delta@(dx:.dy:.()) = unpack (pos - last)
       pos' = mousePos state
       rot = rotationY (toAngle dx) * rotationX (toAngle dy)
       rotMat' = multmm (unpackMat $ rotMat state) rot
  writeIORef controllerState (state { lastPos = pos', rotMat = packMat rotMat' })

  if zoomIn state then modifyIORef zdepth (\x -> x + 0.1) else return ()
  if zoomOut state then modifyIORef zdepth (\x -> x - 0.1) else return () 

  glLoadIdentity  
  --gluLookAt 5 5 5 0 0 0 0 1 0
  let current = viewTrafo state

  let oldForward = forward current
  let yaw' = rotationVec (unpack $ up current) (double2Float $ dx * 0.01)
  let pitch' = rotationVec (unpack $ right current) (double2Float $ dy * 0.01)

  let newForward' = multvm (blowUp oldForward 0.0)  (multmm pitch' yaw') 
  let newTrafo = changeByForward (pack $ V.take n3 newForward') current

  let speed = 10.3
  let newTrafo' = case zoomIn state of
                     True -> moveForward speed newTrafo
                     False -> case zoomOut state of
                                True -> moveForward (-speed) newTrafo
                                False -> newTrafo

  let newTrafo'' = if strafe state /= 0 then strafeView (0.2 * (fromIntegral $ strafe state)) newTrafo'
                    else newTrafo'
  state <- readIORef controllerState
  writeIORef controllerState (state { viewTrafo = newTrafo'' })
 
  let currentTrafo = C.forward $ trafo newTrafo''
  if egoController then multMatrix' (transpose currentTrafo)
   else multMatrix rotMat'

  f <- readIORef filt
  --glBindTexture gl_TEXTURE_2D (texs!!f)
  glEnable gl_TEXTURE_2D
  glEnable gl_LIGHTING
  glEnable gl_CULL_FACE
  --renderObject Solid (SierpinskiSponge 7)
  glDisable gl_LIGHTING
  renderObject Wireframe (Cube 1)
  glEnable gl_LIGHTING

 -- sem_Sg sampleSg  newTrafo'' (C.Trafo3D { C.forward = identity, C.backward = undefined })
  snd sg newTrafo''

 {- 
  glBegin gl_QUADS -- start drawing a polygon (4 sided)
  -- first the front
  glNormal3f     0    0    1
  glTexCoord2f   0    0 
  glVertex3f   (-1) (-1)   1  -- bottom left of quad (Front)
  glTexCoord2f   1    0 
  glVertex3f     1  (-1)   1  -- bottom right of quad (Front)
  glTexCoord2f   1    1 
  glVertex3f     1    1    1  -- top right of quad (Front)
  glTexCoord2f   0    1 
  glVertex3f   (-1)   1    1  -- top left of quad (Front)
  -- now the back
  glNormal3f     0    0  (-1)
  glTexCoord2f   1    0 
  glVertex3f   (-1) (-1) (-1) -- bottom right of quad (Back)
  glTexCoord2f   1    1 
  glVertex3f   (-1)   1  (-1) -- top right of quad (Back)
  glTexCoord2f   0    1 
  glVertex3f     1    1  (-1) -- top left of quad (Back)
  glTexCoord2f   0    0 
  glVertex3f     1  (-1) (-1) -- bottom left of quad (Back)
  -- now the top
  glNormal3f     0    1    0
  glTexCoord2f   0    1
  glVertex3f   (-1)   1  (-1) -- top left of quad (Top)
  glTexCoord2f   0    0  
  glVertex3f   (-1)   1    1  -- bottom left of quad (Top)
  glTexCoord2f   1    0  
  glVertex3f     1    1    1  -- bottom right of quad (Top)
  glTexCoord2f   1    1  
  glVertex3f     1    1  (-1) -- top right of quad (Top)
  -- now the bottom
  glNormal3f     0  (-1)   0
  glTexCoord2f   1    1  
  glVertex3f     1  (-1)   1  -- top right of quad (Bottom)
  glTexCoord2f   0    1  
  glVertex3f   (-1) (-1)   1  -- top left of quad (Bottom)
  glTexCoord2f   0    0  
  glVertex3f   (-1) (-1) (-1) -- bottom left of quad (Bottom)
  glTexCoord2f   1    0  
  glVertex3f     1  (-1) (-1) -- bottom right of quad (Bottom)
  -- now the right
  glNormal3f     1    0    0
  glTexCoord2f   1    0  
  glVertex3f     1  (-1) (-1) -- bottom right of quad (Right)
  glTexCoord2f   1    1  
  glVertex3f     1    1  (-1) -- top right of quad (Right)
  glTexCoord2f   0    1  
  glVertex3f     1    1    1  -- top left of quad (Right)
  glTexCoord2f   0    0
  glVertex3f     1  (-1)   1  -- bottom left of quad (Right)
  -- now the left
  glNormal3f   (-1)   0    1
  glTexCoord2f   0    0  
  glVertex3f   (-1) (-1) (-1) -- bottom left of quad (Left)
  glTexCoord2f   1    0  
  glVertex3f   (-1)   1  (-1) -- top left of quad (Left)
  glTexCoord2f   1    1  
  glVertex3f   (-1)   1    1  -- top right of quad (Left)
  glTexCoord2f   0    1  
  glVertex3f   (-1) (-1)   1  -- bottom right of quad (Left)

  glEnd -}

  xsp <- readIORef xspeed
  ysp <- readIORef yspeed  
  writeIORef xrot $! xr + xsp
  writeIORef yrot $! yr + ysp
  
  glFlush

  end <- getTime clock
  glFlush

  let diff = (sec end - sec start) * 1000000000 + (nsec end - nsec start)
  if mod (cycleCount state) 10 == 0 then
    print $ 1000000000.0 / fromIntegral diff
    else return ()
  modifyIORef controllerState (\s -> s { cycleCount = cycleCount s + 1} )

shutdown :: GLFW.WindowCloseCallback
shutdown = do
  GLFW.closeWindow
  GLFW.terminate
  _ <- exitWith ExitSuccess
  return True

mousePressed :: IORef ControllerState -> GLFW.MouseButton -> Bool -> IO ()
mousePressed state GLFW.MouseButton0 pressed = modifyIORef state (\s -> s { mouseDown = pressed }) 
mousePressed _ _ _ = putStrLn "not implemented"

keyPressed :: IORef Bool -> IORef Bool -> IORef Int -> IORef GLfloat
           -> IORef GLfloat -> IORef GLfloat -> IORef ControllerState -> GLFW.KeyCallback
keyPressed _ _ _ _ _ _ _ GLFW.KeyEsc   True = shutdown >> return ()
keyPressed l _ _ _ _ _ _ (GLFW.CharKey 'L') True = do
  le <- readIORef l
  if le == True
    then glEnable  gl_LIGHTING
    else glDisable gl_LIGHTING
  writeIORef l $! not le
keyPressed _ _ filt _ _ _ _ (GLFW.CharKey 'F') True = do
  f <- readIORef filt
  writeIORef filt $! (f + 1) `mod` 3
keyPressed l b f zd xs ys s (GLFW.CharKey 'l') d =
  keyPressed l b f zd xs ys s (GLFW.CharKey 'L') d
keyPressed l b f zd xs ys s (GLFW.CharKey 'f') d =
  keyPressed l b f zd xs ys s (GLFW.CharKey 'F') d
keyPressed l b f zd xs ys s (GLFW.CharKey 'b') d =
  keyPressed l b f zd xs ys s (GLFW.CharKey 'B') d
keyPressed _ b _ _ _ _ _ (GLFW.CharKey 'B') True = do
  bp <- readIORef b
  if bp == True
    then glEnable  gl_BLEND >> glDisable gl_DEPTH_TEST
    else glDisable gl_BLEND >> glEnable  gl_DEPTH_TEST
  writeIORef b $! not bp
keyPressed _ _ _ zdepth _ _ s (GLFW.CharKey 'W') p = modifyIORef s (\s -> s { zoomIn = p })
keyPressed _ _ _ zdepth _ _ s (GLFW.CharKey 'S') p = modifyIORef s (\s -> s { zoomOut = p}) 
keyPressed _ _ _ zdepth _ _ s (GLFW.CharKey 'A') p = modifyIORef s (\s -> s { strafe  = if p then -1 else 0 })
keyPressed _ _ _ zdepth _ _ s (GLFW.CharKey 'D') p = modifyIORef s (\s -> s { strafe = if p then 1 else 0 }) 
keyPressed _ _ _ _ xspeed _ _ GLFW.KeyUp True = do
  xs <- readIORef xspeed
  writeIORef xspeed $! xs - 0.1
keyPressed _ _ _ _ xspeed _ _ GLFW.KeyDown True = do
  xs <- readIORef xspeed
  writeIORef xspeed $! xs + 0.1
keyPressed _ _ _ _ _ yspeed _ GLFW.KeyRight True = do
  xs <- readIORef yspeed
  writeIORef yspeed $! xs + 0.1
keyPressed _ _ _ _ _ yspeed _ GLFW.KeyLeft True = do
  ys <- readIORef yspeed
  writeIORef yspeed $! ys - 0.1
keyPressed _ _ _ _ _ _ _ _ _ = return ()

mousePositionCallback :: IORef ControllerState -> Int -> Int -> IO ()
mousePositionCallback controllerState x y = modifyIORef controllerState (\c -> c { mousePos = Vec2D (fromIntegral x) (fromIntegral y) })

main :: IO ()
main = do
     True <- GLFW.initialize
     initialize "abc" []
     -- select type of display mode:
     -- Double buffer
     -- RGBA color
     -- Alpha components supported
     -- Depth buffer
     let dspOpts = GLFW.defaultDisplayOptions
                     -- get a 800 x 600 window
                     { GLFW.displayOptions_width  = 800
                     , GLFW.displayOptions_height = 600
                     -- Set depth buffering and RGBA colors
                     , GLFW.displayOptions_numRedBits   = 5
                     , GLFW.displayOptions_numGreenBits = 6
                     , GLFW.displayOptions_numBlueBits  = 5
                     , GLFW.displayOptions_numAlphaBits = 0
                     , GLFW.displayOptions_numDepthBits = 24
                     , GLFW.displayOptions_displayMode = GLFW.Window
                     }
     -- open a window
     True <- GLFW.openWindow dspOpts
     -- window starts at upper left corner of the screen
     GLFW.setWindowPosition 0 0
     GLFW.setWindowTitle "Jeff Molofee's GL Code Tutorial ... NeHe '99"
     lighting <- newIORef True
     blending <- newIORef True
     xrot     <- newIORef (0::GLfloat)
     yrot     <- newIORef (0::GLfloat)
     xspeed   <- newIORef (0::GLfloat)
     yspeed   <- newIORef (0::GLfloat)
     zdepth   <- newIORef (-5.0 :: GLfloat)
     filt     <- newIORef (0::Int)
 
-- (-300):.600:.600:.()) (V.pack $ -301:.599:.601:.())
     let t = lookAt (pack $ (-300):.600:.600:.()) (pack $ (-301):.599:.601:.()) (pack $ 0:.1:.0:.())
 
     controllerState <- newIORef $ ControllerState { 
                                     mouseDown = False, mousePos = Vec2D 0 0, 
                                     lastPos = Vec2D 0 0,
                                     rotMat = packMat identity,
                                     zoomIn = False,
                                     zoomOut = False,
                                     viewTrafo = t, strafe = 0, cycleCount = 0 }

     -- initialize our window.
     texs <- initGL
     GLFW.setWindowRefreshCallback
       (drawScene texs xrot yrot xspeed yspeed zdepth filt controllerState)
     -- register the funciton called when our window is resized
     GLFW.setWindowSizeCallback resizeScene
     -- register the function called when the keyboard is pressed.
     GLFW.setKeyCallback $
       keyPressed lighting blending filt zdepth xspeed yspeed controllerState
     GLFW.setMouseButtonCallback $ mousePressed controllerState
     GLFW.setMousePositionCallback $ mousePositionCallback controllerState
     GLFW.setWindowCloseCallback shutdown
     forever $ do
       drawScene texs xrot yrot xspeed yspeed zdepth filt controllerState
       GLFW.swapBuffers

