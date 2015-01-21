

-- UUAGC 0.9.42.1 (Diff.ag)

-- List --------------------------------------------------------
data List = Nil
          | Cons (Float) (List)
          deriving ( Show)
-- cata
sem_List (Nil) =
    (sem_List_Nil)
sem_List (Cons _head _tail) =
    (sem_List_Cons _head (sem_List _tail))
sem_List_Nil =
    (\ _lhsIavg ->
         (let _lhsOlength =
                  0.0
              _lhsOsum =
                  0
              _lhsOres =
                  Nil
          in  ( _lhsOlength,_lhsOres,_lhsOsum)))
sem_List_Cons head_ tail_ =
    (\ _lhsIavg ->
         (let _lhsOlength =
                  1.0 + _tailIlength
              _lhsOsum =
                  head_ + _tailIsum
              _tailOavg =
                  _lhsIavg
              _lhsOres =
                  Cons (head_ - _lhsIavg) _tailIres
              ( _tailIlength,_tailIres,_tailIsum) =
                  tail_ _tailOavg
          in  ( _lhsOlength,_lhsOres,_lhsOsum)))
-- Root --------------------------------------------------------
data Root = Root (List)
-- cata
sem_Root (Root _list) =
    (sem_Root_Root (sem_List _list))
sem_Root_Root list_ =
    (let _listOavg =
             _listIsum / _listIlength
         _lhsOres =
             _listIres
         ( _listIlength,_listIres,_listIsum) =
             list_ _listOavg
     in  ( _lhsOres))