

-- UUAGC 0.9.42.1 (ag1.ag)
module ag1 where
-- List --------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         avg                  : Float
      synthesized attributes:
         length               : Float
         sum                  : Float
   alternatives:
      alternative Nil:
      alternative Cons:
         child head           : {Float}
         child tail           : List 
-}
data List = List_Nil {}
          | List_Cons {head_List_Cons :: Float,tail_List_Cons :: List}
-- cata
sem_List :: List ->
            T_List
sem_List (List_Nil) =
    (sem_List_Nil)
sem_List (List_Cons _head _tail) =
    (sem_List_Cons _head (sem_List _tail))
-- semantic domain
type T_List = Float ->
              ( Float,Float)
sem_List_Nil :: T_List
sem_List_Nil =
    (\ _lhsIavg ->
         (let _lhsOlength :: Float
              _lhsOsum :: Float
              -- "./ag1.ag"(line 10, column 8)
              _lhsOlength =
                  0.0
              -- "./ag1.ag"(line 15, column 9)
              _lhsOsum =
                  0
          in  ( _lhsOlength,_lhsOsum)))
sem_List_Cons :: Float ->
                 T_List ->
                 T_List
sem_List_Cons head_ tail_ =
    (\ _lhsIavg ->
         (let _lhsOlength :: Float
              _lhsOsum :: Float
              _tailOavg :: Float
              _tailIlength :: Float
              _tailIsum :: Float
              -- "./ag1.ag"(line 11, column 9)
              _lhsOlength =
                  1.0 + _tailIlength
              -- "./ag1.ag"(line 16, column 10)
              _lhsOsum =
                  head_ + _tailIsum
              -- "./ag1.ag"(line 23, column 9)
              _tailOavg =
                  _lhsIavg
              ( _tailIlength,_tailIsum) =
                  tail_ _tailOavg
          in  ( _lhsOlength,_lhsOsum)))
-- Root --------------------------------------------------------
{-
   alternatives:
      alternative Root:
         child list           : List 
-}
data Root = Root_Root {list_Root_Root :: List}
-- cata
sem_Root :: Root ->
            T_Root
sem_Root (Root_Root _list) =
    (sem_Root_Root (sem_List _list))
-- semantic domain
type T_Root = ( )
sem_Root_Root :: T_List ->
                 T_Root
sem_Root_Root list_ =
    (let _listOavg :: Float
         _listIlength :: Float
         _listIsum :: Float
         -- "./ag1.ag"(line 20, column 9)
         _listOavg =
             _listIsum / _listIlength
         ( _listIlength,_listIsum) =
             list_ _listOavg
     in  ( ))