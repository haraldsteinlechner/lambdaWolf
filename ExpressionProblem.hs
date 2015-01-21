{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module ExpressionProblem where

import Debug.Trace
import Data.Monoid
import Data.Dynamic
import Data.HList.TypeCastGeneric2


data VertexGeometry = VertexGeometry String -- VertexGeometries are strings
data VertexGeometrySet = VertexGeometrySet [VertexGeometry]
data Thing = forall a . Sg a => MkThing a -- type destruction ala carte
type DifferentThings = [Thing] -- a heterogenous collection of SGs
data HGroup = HGroup DifferentThings -- a group is a collection of things
data Group a = Group [a]  -- a homegeonous group

renderVg :: VertexGeometry -> String
renderVg (VertexGeometry s) =s

-- base type for scenegraphs
class Sg a where
    children :: a -> DifferentThings

-- make it possible to occur in scenegraphs
instance Sg VertexGeometrySet where children _ = []
instance Sg HGroup where
    children (HGroup xs) = xs
instance Sg a => Sg (Group a) where 
    children (Group xs) = map pack xs

-- introduce new traversal

class Sg a => Render a where
    -- provide default implementation if node has no semantics respective to render
    render :: a -> String
    render = renderThings . children 

-- scene graphs are rendererable. use default implementation if no other
-- provided

instance Sg a => Render a 

-- however vgsets should be rendererd directly

instance Render VertexGeometrySet where
    -- render everything sequentially.
    render (VertexGeometrySet vgs) = concatMap renderVg vgs 

-- Groups are renderable
instance Render HGroup 


pack ::Render a => a -> Thing
pack = MkThing

renderThings :: [Thing] -> String
renderThings = mconcat . map f 
  where f (MkThing x) = render x

test1 = HGroup [ pack $ VertexGeometrySet [VertexGeometry "abc" ],
                 pack $ VertexGeometrySet [VertexGeometry "cdef"]  ] 

rendering1 = render test1 :: String

test2 =  Group [ VertexGeometrySet [VertexGeometry "abc" ],
                 VertexGeometrySet [VertexGeometry "cdef"]  ] 

rendering2 = render test2 :: String

-- weird type stuff tests from now

class Print a where
    mprint :: a -> IO ()

class Print' flag a where
    mprint' :: flag -> a -> IO ()
 
instance (ShowPred a flag, Print' flag a) => Print a where
    mprint = mprint' (undefined::flag)

class ShowPred a flag | a->flag where {}
instance TypeCast flag HFalse => ShowPred a flag

instance ShowPred Int  HTrue   -- These instances must be
instance ShowPred Bool HTrue   -- the same as Show's


data HTrue
data HFalse

instance (Show a) => Print' HTrue a where
   mprint' _ x = putStrLn (show x)
instance Print' HFalse a where
   mprint' _ x = putStrLn "No show method"


test5 = toDyn ("abc" :: String)
test6 = case fromDynamic test5 of
          Nothing -> "blubber"
          Just x -> show (x::String)
