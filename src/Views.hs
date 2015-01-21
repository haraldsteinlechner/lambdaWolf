{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
module Views where

import qualified Data.Map as M
import Data.Map(Map)
import Data.Maybe
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Identity
import Control.Monad.Identity
import Control.Applicative
import Data.List hiding (insert)
import Test.QuickCheck

data Color = Red | Green | Blue deriving (Show,Eq,Ord)
type Position = Int

class Colored a where
    color :: a -> Color

class Positioned a where
    position :: a -> Position

data Obj where
    Obj :: Color -> Position -> Obj
  deriving(Show,Eq,Ord)

instance Colored Obj where
    color (Obj c _) = c

instance Positioned Obj where
    position (Obj _ p) = p

someObjects :: [Obj]
someObjects = [Obj Red 1, Obj Green 2, Obj Blue 3]

type Scene = [Obj]
type SceneM m a = StateT Scene m a

newtype SceneDB m a = SceneDB (SceneM m a) deriving (Monad)

insert :: Monad m => Obj -> SceneDB m ()
insert obj = SceneDB $ modify (obj :) 

query :: Monad m => (Scene -> Scene) -> SceneDB m Scene
query f = SceneDB $ liftM f get

runScene :: Monad m => SceneDB m Scene -> m Scene
runScene (SceneDB m) = evalStateT m []

runSceneId ::  SceneDB Identity Scene -> Scene
runSceneId = runIdentity . runScene

myScene = do
    insert $ Obj Red 1
    insert $ Obj Green 2
    insert $ Obj Blue 3 


redObjects :: Monad m => SceneDB m Scene
redObjects = query redOnes
  where redOnes s = [obj | obj <- s, isRed obj]
        isRed = (== Red) . color

testScene1 = runSceneId $ myScene >> redObjects

data Expr a where
    NumOp :: (Num a, Show a) => (a -> a -> a) -> Expr a -> Expr a -> Expr a
    Equals :: (Eq a, Show a) => Expr a -> Expr a -> Expr Bool
    Constant :: Show a => a -> Expr a
    If :: (Show a) => Expr Bool -> Then (Expr a) -> Else (Expr a) -> Expr a 
    Call :: (Expr a -> Expr b) -> Expr a -> Expr b
    Match :: Expr a -> With [(Expr Bool, Expr b)] -> Expr b

deriving instance Show (Expr a)

instance Show (a -> b) where show _ = "hfun"

newtype Then a = Then a deriving (Show)
newtype Else a = Else a deriving (Show)
newtype With a = With a deriving (Show)

add = NumOp (+)
minus = NumOp (-)


eval :: Expr a -> a
eval (NumOp f x y) = eval x `f` eval y
eval (Constant x) = x
eval (If b (Then t) (Else f)) = if eval b then eval t else eval f
eval (Call f a) = eval $ f a
eval (Equals a b) = (eval a) == (eval b)
eval (Match e (With xs)) = let (_,((_,c):_)) = break ((== True) . eval . fst) xs
                           in eval c

testA = If (Constant True) (Then $ Constant 1) (Else $ Constant 2)

plus1 = add (Constant 1)
onePlus1 = Call plus1 (Constant 1)

aSum = foldr add (Constant 0) $ map Constant [1..10]
qTest = eval aSum == sum [1..10]

--mEq a b = Call (Lift (== eval a)

aFib :: Expr Int -> Expr Int 
aFib n = Match n $ With [
                         (n `Equals` Constant 0, Constant 0),
                         (n `Equals` Constant 1, Constant 1),
                         (Constant True, add (aFib n') (aFib n'')) ]
    where n'  = n `minus` Constant 1
          n'' = n `minus` Constant 2

fibTest n | n < 0 = True
          | n < 30 = True
          | otherwise = ( eval $ aFib (Constant n) ) == fib n

fib n' = case n' of 
          0 -> 0
          1 -> 1
          n -> fib (n'-1)  + fib (n'-2)

data Tree a b = Node a (Tree a b) (Tree a b)
              | Leaf b | TNil deriving (Show,Eq)

mkTree :: Ord a => [a] -> Tree a a
mkTree = mkTree' . sort 

mkTree' :: Ord a => [a] -> Tree a a
mkTree' (x:[]) = Leaf x
mkTree' xs = Node (xs!!half) (mkTree' b) (mkTree' e)
    where (b,e) = splitAt half xs
          half = length xs `div`  2


