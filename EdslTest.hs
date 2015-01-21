{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test where

import Data.List
import Debug.Trace as T
import Control.Applicative 
import Control.Monad
import Control.Monad.Trans
import Text.Show.Functions

binaryOpVec op (x,y,z) (x',y',z') =
   (x `op` x',y `op` y',z `op`z')

instance Num Vec3 where
  (+) = binaryOpVec (+)
  (*) = binaryOpVec (*)
  abs = undefined
  signum = undefined
  fromInteger = undefined

{- Syntax with some static semantics -} 

type Vec3 = (Float,Float,Float)

data Op = Mul | Add deriving (Show,Eq)
data CmpOp = Less deriving (Show,Eq)

data Func a b = Func { func :: Expr a -> Expr b }

data Value a = Value a

instance Show (Value a) where
  show = const "<value>"

instance Show (Func a b) where
  show = const "func"

data Expr a where
  Vec   :: Vec3 -> Expr Vec3
  Bool  :: Bool -> Expr Bool
  Lift  :: Value a -> Expr a
  Num   :: (Num a, Show a) => a -> Expr a
  NumOp :: (Num a) => Op -> Expr a -> Expr a -> Expr a
  NumUn :: String -> Expr a -> Expr a
  Cmp   :: (Ord a) => CmpOp -> Expr a -> Expr a -> Expr Bool
  Apply :: Func a b -> Expr a -> Expr b
  If    :: Expr Bool -> Expr a -> Expr a -> Expr a
  Print :: String -> Expr a -> Expr a
  Intrinsic :: String -> [Evaluatable Expr] -> Expr a
  Cast :: Expr Integer -> Expr Int
 
deriving instance Show (Expr a)


instance Num (Expr Int) where
  (+) = NumOp Add 
  (*) = NumOp Mul
  abs = NumUn "abs"
  signum = NumUn "signum"
  fromInteger x = (Cast . Lift . Value) 4 

instance Num a => Num (Expr a) where
  (+) = NumOp Add 
  (*) = NumOp Mul
  abs = NumUn "abs"
  signum = NumUn "signum"
  fromInteger = error . show


add :: Num a => Expr a -> Expr a -> Expr a
add = NumOp Add

mul :: Num a => Expr a -> Expr a -> Expr a
mul = NumOp Mul

less :: (Ord a) => Expr a -> Expr a -> Expr Bool
less = Cmp Less 

-- keep Num - use light weight overloads
(<+>) :: Num a => Expr a -> Expr a -> Expr a
x <+> y = add x y

(<.>) :: Func a b -> Expr a -> Expr b
f <.> arg = func f arg

true :: Expr Bool
true = Bool True

printIO ::  String -> Expr a -> Expr a
printIO = Print 

call :: String -> [Evaluatable Expr] -> Expr a
call = Intrinsic

call' :: (Show a, Show b, Show c) => String -> (Expr b, Expr c) -> Expr a
call' s (a1,a2) = Intrinsic s [pack a1,pack a2]

-- foreign calls need multiparam functions with heterogenous argument lists:
--
class Lang e where
  evalExpr :: e a -> a
  showExpr :: e a -> String

instance Lang Expr where
  evalExpr = eval
  showExpr = (++ " Done.") . show

data Evaluatable e = forall b . (Lang e, Show (e b), Show b) => MkExpr (e b)
deriving instance Show (Evaluatable e)

pack ::Show a => Expr a -> Evaluatable Expr
pack = MkExpr

-- fix this ugly shit
infixr 0 <..>
(<..>) (a,b) = [pack a, pack b] 



{- Semantics -} 

sem :: (Num a) => Op -> a -> a -> a
sem Mul = (*)
sem Add = (+)

semCmp :: (Ord a) => CmpOp -> a -> a -> Bool
semCmp Less = (<)

evalNum :: (Num a) => Op -> Expr a -> Expr a -> a
evalNum op' x y = eval x `op` eval y
  where op = sem op'

evalCmp :: (Ord a) => CmpOp -> Expr a -> Expr a -> Bool
evalCmp op' x y = eval x `op` eval y
  where op = semCmp op'

eval :: Expr a -> a
eval (Vec x) = x
eval (Bool x) = x
eval (Num x) = x
eval (NumOp op x y) = evalNum op x y
eval (Apply f arg)   = eval $ func f arg
eval (If b t e) = if eval b then eval t else eval e
eval (Cmp op x y) = evalCmp op x y
eval (Print s expr) = eval (T.trace s expr)
eval (Intrinsic f args) = let argVals = map evalUnpack args
                              conc = intersperse "," argVals
                          in error $ "could not evaluate. generate code like: "
                                      ++ f ++ "(" ++ concat conc ++ ")"
eval (Lift (Value a)) = a
eval (Cast x) = fromInteger (eval x)
                          
--f ++ "(" ++ (intersperse "," (map evalUnpack args)) ++")"

{- Tests -}

customFunc :: Func Vec3 Vec3
customFunc = Func f
  where f = add (Vec (1,1,1))

test1 ::  Expr Vec3
test1 = let x = Vec (0,0,0)
            y = Vec (1,1,1)
            z = x + y
            blubber = customFunc <.> Vec (100,100,100)
        in blubber + z

test2 :: Func Bool Vec3
test2 = Func expr
  where expr a = If a (Vec (0,0,0)) (Vec (110,10,10))

part :: Expr Vec3
part = test2 <.> true

lessTest ::  Expr Bool
lessTest = less (Num 10) (Num 6)

count :: (Num a, Ord a, Show a) => Expr a -> Expr a
count x = let b = x `less` Num 1
              continue = count $ x <+> Num (-1)
          in printIO "evaluating.." $ If b (Num 666) continue

testIO = eval (count $ Num 20)

testCall ::  Expr String
testCall = call' "someStrangeFunction" (Num 1,Num 2)

tests = [ pack test1
        , pack part
        , pack lessTest
        , pack $ count (Num 20)
        , pack testCall] 

testAll ::  IO ()
testAll = mapM_ print' tests
  where print' x = do putStrLn $ "expr: " ++ showUnpack x
                      putStrLn $ "==> " ++ evalUnpack x ++ "\n\n"

showUnpack (MkExpr e) = take 200 (showExpr e) ++ " (possibly truncated)"
evalUnpack (MkExpr e) = show $ evalExpr e


-- frontend stuff

--instance Show (Expr a) where
--  show = const "expr"
--
putSomething :: String -> Expr ()
putSomething str = T.trace str $ Lift (Value ())

data RealWorld = RealWorld

instance Functor Expr where
  fmap f = Lift . Value . f . eval

instance Monad Expr where
  (>>=) a b = b (eval a)
  return = Lift . Value

newtype ExprT m a = ExprT { runExprT :: m (Expr a) }

instance Monad m => Monad (ExprT m) where
  return = ExprT . return . Lift . Value
  (>>=) a b = ExprT $ runExprT a >>= runExprT . b . eval

instance MonadTrans ExprT where
  lift m = ExprT $ liftM (Lift . Value) m

instance MonadIO m => MonadIO (ExprT m) where
  liftIO m = lift (liftIO m)

type ExprIO a = ExprT IO a

evalIO :: ExprIO a -> IO (Expr a)
evalIO = runExprT 

functorTest = (*2) <$> (Num 5)

monadTest = do x <- Vec (0,0,0)
               putSomething $ "value: " ++ (show x)
               return $ Vec (5,5,5)

forTest :: Expr ()
forTest = forM_ [Num 5, Num 6] effect
  where effect x = putSomething $ "value" ++ (show x)

forTest2 :: ExprT IO ()
forTest2 = forM_ [Num 5, Num 6] effect

effect :: (Show a) => a -> ExprT IO ()
effect x = liftIO $ putStrLn ("val: " ++ show x)

filter1 :: [Expr Float] -> Expr Float
filter1 xs = let sumUp = foldr (<+>) (Num 0) 
             in sumUp xs `mul` Num 3


test3 :: Monad m => ExprT m Float -> ExprT m Float
test3 a = do x <- a
             expr $ Num 6 - Num x

expr :: Monad m => Expr a -> ExprT m a
expr = return . eval

test4 = -(Num (3::Int))
