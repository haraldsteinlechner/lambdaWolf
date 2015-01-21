

-- UUAGC 0.9.38.1 (BenchAg.ag)
module BenchAg where
{-# LINE 21 "BenchAg.ag" #-}


import Primitives

{-# LINE 11 "BenchAg.hs" #-}
-- Sg ----------------------------------------------------------
data Sg  = Leaf (Geometry) 
         | Nil 
         | Node (Sg ) (Sg ) 
         | TrafoApp (Sg ) (Trafo3) 
-- cata
sem_Sg (Leaf _geometry )  =
    (sem_Sg_Leaf _geometry )
sem_Sg (Nil )  =
    (sem_Sg_Nil )
sem_Sg (Node _left _right )  =
    (sem_Sg_Node (sem_Sg _left ) (sem_Sg _right ) )
sem_Sg (TrafoApp _sg _trafo )  =
    (sem_Sg_TrafoApp (sem_Sg _sg ) _trafo )
sem_Sg_Leaf geometry_  =
    (\ _lhsImodelTrafo ->
         (let _lhsOrenderJobs =
                  ({-# LINE 16 "BenchAg.ag" #-}
                   getRenderJob geometry_
                   {-# LINE 31 "BenchAg.hs" #-}
                   )
          in  ( _lhsOrenderJobs)))
sem_Sg_Nil  =
    (\ _lhsImodelTrafo ->
         (let _lhsOrenderJobs =
                  ({-# LINE 17 "BenchAg.ag" #-}
                   0
                   {-# LINE 39 "BenchAg.hs" #-}
                   )
          in  ( _lhsOrenderJobs)))
sem_Sg_Node left_ right_  =
    (\ _lhsImodelTrafo ->
         (let _lhsOrenderJobs =
                  ({-# LINE 15 "BenchAg.ag" #-}
                   _leftIrenderJobs + _rightIrenderJobs
                   {-# LINE 47 "BenchAg.hs" #-}
                   )
              _leftOmodelTrafo =
                  ({-# LINE 11 "BenchAg.ag" #-}
                   _lhsImodelTrafo
                   {-# LINE 52 "BenchAg.hs" #-}
                   )
              _rightOmodelTrafo =
                  ({-# LINE 11 "BenchAg.ag" #-}
                   _lhsImodelTrafo
                   {-# LINE 57 "BenchAg.hs" #-}
                   )
              ( _leftIrenderJobs) =
                  left_ _leftOmodelTrafo 
              ( _rightIrenderJobs) =
                  right_ _rightOmodelTrafo 
          in  ( _lhsOrenderJobs)))
sem_Sg_TrafoApp sg_ trafo_  =
    (\ _lhsImodelTrafo ->
         (let _lhsOrenderJobs =
                  ({-# LINE 18 "BenchAg.ag" #-}
                   _sgIrenderJobs
                   {-# LINE 69 "BenchAg.hs" #-}
                   )
              _sgOmodelTrafo =
                  ({-# LINE 20 "BenchAg.ag" #-}
                   multiply trafo_ _lhsImodelTrafo
                   {-# LINE 74 "BenchAg.hs" #-}
                   )
              ( _sgIrenderJobs) =
                  sg_ _sgOmodelTrafo 
          in  ( _lhsOrenderJobs)))