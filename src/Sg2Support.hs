{-# LANGUAGE Arrows #-}

module Sg2Support where

import Control.Monad
import qualified AbstractCamera as C
import qualified Data.Vec as V
import Camera
import FRP.Yampa
import FRP.Yampa.GLUT.Adapter
import Control.Arrow
import Reactive

type TrafoAction = C.Trafo -> C.Trafo
type LodDecider = ViewTrafo -> C.Trafo -> BoundingBox -> Bool
type BoundingBox = (V.Vec3 Float,V.Vec3 Float)

type Viewport = Int
type CameraSF = SF (Event UI) ViewTrafo

composeBoundingBox :: BoundingBox -> BoundingBox -> BoundingBox
composeBoundingBox (min, max) (min',max') = (V.zipWith Prelude.min min min', 
                                             V.zipWith Prelude.max max max')

emptyBoundingBox :: BoundingBox
emptyBoundingBox = (V.unpack $ V.Vec3F 0 0 0,V.unpack $ V.Vec3F 0 0 0)

createBoundingBox :: C.Trafo -> BoundingBox
createBoundingBox trafo = (center,center)
 where center = V.take V.n3 $ V.column V.n3 (C.forward trafo)

type RenderSF = SF (Event UI) (IO ())

composeSF :: RenderSF -> RenderSF -> RenderSF 
composeSF = undefined

nilRenderSF :: RenderSF 
nilRenderSF = undefined


{-
type RawRenderAction = ViewTrafo -> IO ()
type RenderAction = C.Trafo -> ViewTrafo -> IO ()
type TrafoAction = C.Trafo -> C.Trafo
type LodDecider = ViewTrafo -> C.Trafo -> BoundingBox -> Bool
type BoundingBox = (V.Vec3 Float,V.Vec3 Float)

composeRenderAction :: RawRenderAction -> RawRenderAction -> RawRenderAction
composeRenderAction first second initialTrafo = do first initialTrafo
                                                   second initialTrafo

renderLod :: RawRenderAction -> RawRenderAction -> C.Trafo -> LodDecider -> BoundingBox -> RawRenderAction
renderLod high low trafo decider bb viewTrafo = do
 if decider viewTrafo trafo bb
  then high viewTrafo
  else low viewTrafo

nilRenderAction :: RawRenderAction
nilRenderAction _ = return ()
-}

