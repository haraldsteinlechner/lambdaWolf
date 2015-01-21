

-- UUAGC 0.9.42.1 (Sg.ag)
module Sg where
{-# LINE 38 "./Sg.ag" #-}

{-# LANGUAGE Arrows #-}

import Control.Monad
import qualified AbstractCamera as C
import qualified Data.Vec as V
import Camera
import FRP.Yampa
import FRP.Yampa.GLUT.Adapter
import Control.Arrow

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
{-# LINE 53 "Sg.hs" #-}
-- Sg ----------------------------------------------------------
data Sg = Node (Sg) (Sg)
        | Trafo (Sg) (TrafoAction)
        | Leaf (RenderAction)
        | Lod (Sg) (Sg) (LodDecider)
        | Nil
-- cata
sem_Sg (Node _left _right) =
    (sem_Sg_Node (sem_Sg _left) (sem_Sg _right))
sem_Sg (Trafo _left _trafo) =
    (sem_Sg_Trafo (sem_Sg _left) _trafo)
sem_Sg (Leaf _renderAction) =
    (sem_Sg_Leaf _renderAction)
sem_Sg (Lod _low _high _lodDecider) =
    (sem_Sg_Lod (sem_Sg _low) (sem_Sg _high) _lodDecider)
sem_Sg (Nil) =
    (sem_Sg_Nil)
sem_Sg_Node left_ right_ =
    (\ _lhsItrafo ->
         (let _lhsOrenderAction =
                  ({-# LINE 19 "./Sg.ag" #-}
                   composeRenderAction _leftIrenderAction _rightIrenderAction
                   {-# LINE 76 "Sg.hs" #-}
                   )
              _leftOtrafo =
                  ({-# LINE 26 "./Sg.ag" #-}
                   _lhsItrafo
                   {-# LINE 81 "Sg.hs" #-}
                   )
              _rightOtrafo =
                  ({-# LINE 27 "./Sg.ag" #-}
                   _lhsItrafo
                   {-# LINE 86 "Sg.hs" #-}
                   )
              _lhsOboundingBox =
                  ({-# LINE 30 "./Sg.ag" #-}
                   composeBoundingBox _leftIboundingBox _rightIboundingBox
                   {-# LINE 91 "Sg.hs" #-}
                   )
              _lhsOrenderSF =
                  ({-# LINE 35 "./Sg.ag" #-}
                   composeSF _leftIrenderSF _rightIrenderSF
                   {-# LINE 96 "Sg.hs" #-}
                   )
              ( _leftIboundingBox,_leftIrenderAction,_leftIrenderSF) =
                  left_ _leftOtrafo
              ( _rightIboundingBox,_rightIrenderAction,_rightIrenderSF) =
                  right_ _rightOtrafo
          in  ( _lhsOboundingBox,_lhsOrenderAction,_lhsOrenderSF)))
sem_Sg_Trafo left_ trafo_ =
    (\ _lhsItrafo ->
         (let _lhsOrenderAction =
                  ({-# LINE 20 "./Sg.ag" #-}
                   _leftIrenderAction
                   {-# LINE 108 "Sg.hs" #-}
                   )
              _leftOtrafo =
                  ({-# LINE 28 "./Sg.ag" #-}
                   trafo_ _lhsItrafo
                   {-# LINE 113 "Sg.hs" #-}
                   )
              _lhsOboundingBox =
                  ({-# LINE 31 "./Sg.ag" #-}
                   _leftIboundingBox
                   {-# LINE 118 "Sg.hs" #-}
                   )
              _lhsOrenderSF =
                  ({-# LINE 16 "./Sg.ag" #-}
                   _leftIrenderSF
                   {-# LINE 123 "Sg.hs" #-}
                   )
              ( _leftIboundingBox,_leftIrenderAction,_leftIrenderSF) =
                  left_ _leftOtrafo
          in  ( _lhsOboundingBox,_lhsOrenderAction,_lhsOrenderSF)))
sem_Sg_Leaf renderAction_ =
    (\ _lhsItrafo ->
         (let _lhsOrenderAction =
                  ({-# LINE 21 "./Sg.ag" #-}
                   renderAction_ _lhsItrafo
                   {-# LINE 133 "Sg.hs" #-}
                   )
              _lhsOboundingBox =
                  ({-# LINE 33 "./Sg.ag" #-}
                   createBoundingBox _lhsItrafo
                   {-# LINE 138 "Sg.hs" #-}
                   )
              _lhsOrenderSF =
                  ({-# LINE 36 "./Sg.ag" #-}
                   undefined
                   {-# LINE 143 "Sg.hs" #-}
                   )
          in  ( _lhsOboundingBox,_lhsOrenderAction,_lhsOrenderSF)))
sem_Sg_Lod low_ high_ lodDecider_ =
    (\ _lhsItrafo ->
         (let _lhsOrenderAction =
                  ({-# LINE 23 "./Sg.ag" #-}
                   renderLod _highIrenderAction _lowIrenderAction _lhsItrafo lodDecider_ _lowIboundingBox
                   {-# LINE 151 "Sg.hs" #-}
                   )
              _lhsOboundingBox =
                  ({-# LINE 15 "./Sg.ag" #-}
                   _highIboundingBox
                   {-# LINE 156 "Sg.hs" #-}
                   )
              _lhsOrenderSF =
                  ({-# LINE 16 "./Sg.ag" #-}
                   _highIrenderSF
                   {-# LINE 161 "Sg.hs" #-}
                   )
              _lowOtrafo =
                  ({-# LINE 14 "./Sg.ag" #-}
                   _lhsItrafo
                   {-# LINE 166 "Sg.hs" #-}
                   )
              _highOtrafo =
                  ({-# LINE 14 "./Sg.ag" #-}
                   _lhsItrafo
                   {-# LINE 171 "Sg.hs" #-}
                   )
              ( _lowIboundingBox,_lowIrenderAction,_lowIrenderSF) =
                  low_ _lowOtrafo
              ( _highIboundingBox,_highIrenderAction,_highIrenderSF) =
                  high_ _highOtrafo
          in  ( _lhsOboundingBox,_lhsOrenderAction,_lhsOrenderSF)))
sem_Sg_Nil =
    (\ _lhsItrafo ->
         (let _lhsOrenderAction =
                  ({-# LINE 24 "./Sg.ag" #-}
                   nilRenderAction
                   {-# LINE 183 "Sg.hs" #-}
                   )
              _lhsOboundingBox =
                  ({-# LINE 32 "./Sg.ag" #-}
                   createBoundingBox _lhsItrafo
                   {-# LINE 188 "Sg.hs" #-}
                   )
              _lhsOrenderSF =
                  ({-# LINE 37 "./Sg.ag" #-}
                   nilRenderSF
                   {-# LINE 193 "Sg.hs" #-}
                   )
          in  ( _lhsOboundingBox,_lhsOrenderAction,_lhsOrenderSF)))