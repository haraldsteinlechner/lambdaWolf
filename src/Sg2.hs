

-- UUAGC 0.9.42.1 (Sg2.ag)
module Sg2 where
{-# LINE 24 "./Sg2.ag" #-}

import Sg2Support

type Viewport = Int
type CameraSF = Int

{-# LINE 13 "Sg2.hs" #-}
-- Camera ------------------------------------------------------
data Camera = Camera (CameraSF) (Sg)
-- cata
sem_Camera (Camera _trafo _sg) =
    (sem_Camera_Camera _trafo (sem_Sg _sg))
sem_Camera_Camera trafo_ sg_ =
    (let ( _sgIrenderSF) =
             sg_
     in  ( ))
-- RenderTarget ------------------------------------------------
data RenderTarget = RenderTarget (View) (Viewport)
-- cata
sem_RenderTarget (RenderTarget _imgs _properties) =
    (sem_RenderTarget_RenderTarget (sem_View _imgs) _properties)
sem_RenderTarget_RenderTarget imgs_ properties_ =
    (let
     in  ( ))
-- Sg ----------------------------------------------------------
data Sg = Node (Sg) (Sg)
        | Nil
-- cata
sem_Sg (Node _left _right) =
    (sem_Sg_Node (sem_Sg _left) (sem_Sg _right))
sem_Sg (Nil) =
    (sem_Sg_Nil)
sem_Sg_Node left_ right_ =
    (let _lhsOrenderSF =
             ({-# LINE 21 "./Sg2.ag" #-}
              composeSF _leftIrenderSF _rightIrenderSF
              {-# LINE 43 "Sg2.hs" #-}
              )
         ( _leftIrenderSF) =
             left_
         ( _rightIrenderSF) =
             right_
     in  ( _lhsOrenderSF))
sem_Sg_Nil =
    (let _lhsOrenderSF =
             ({-# LINE 22 "./Sg2.ag" #-}
              nilRenderSF
              {-# LINE 54 "Sg2.hs" #-}
              )
     in  ( _lhsOrenderSF))
-- View --------------------------------------------------------
data View = View (Camera)
-- cata
sem_View (View _camera) =
    (sem_View_View (sem_Camera _camera))
sem_View_View camera_ =
    (let
     in  ( ))