module Main where

import Primitives
import BenchAg
import Control.Monad
import Control.Monad.State
import Control.Applicative
import Data.List
import Control.DeepSeq

mkGeometries n = replicateM n mkRandomGeometry 

data Environment = Environment { modelTrafo :: Trafo3 }
type EnvM m = StateT Environment m

stackTrafo :: (Monad m, Applicative m) => Trafo3 -> EnvM m ()
stackTrafo t = do
  current <- modelTrafo <$> get
  put $ Environment { modelTrafo = multiply current t } 

render :: (Monad m, Applicative m) => Sg -> EnvM m Int
render (Leaf g) = return (getRenderJob g)
render (Node l r) = do 
                      l' <- render l
                      r' <- render r
                      return (l' + r')
render (TrafoApp c t) = do
    old <- get
    stackTrafo t
    result <- render c
    put old
    return result
render Nil = return 0

traverse :: Sg -> IO ()
traverse (Leaf g) = g `seq` (return ())
traverse (Node l r) = traverse l >> traverse r
traverse (TrafoApp c t) = c `seq` traverse c
traverse Nil = return ()

main = do
    geometries <- mkGeometries 100000
    let sg = foldl' (\a l -> Node a l) Nil (map Leaf geometries)
    --traverse sg
    renderResult <- evalStateT (render sg) Environment { modelTrafo = identityTrafo } 
    print renderResult
    --let renderResult' = sem_Sg sg identityTrafo
    --print renderResult'
