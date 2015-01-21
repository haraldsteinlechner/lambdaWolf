{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
module SgFrontendTest where

import Control.Monad
import Control.Monad.State
import Data.HList

data Draw = Draw 
newtype Resource a = Resource a 

class Attrib a

instance Attrib (Resource a)  
instance Attrib a => Attrib [a] 

data RenderState = A
                    
{-
- myStyle = style $
-       module Default
-       {
-         RenderState = do diffuseColorTexture $= texA
-                          shader $= shaderA
-         module House
-         {
-           module Window
-           {
-              RenderState = do shader $= mirror
-           }
-           module Wall
-           {
-              RenderState = do shader $= dirt
-           }
-         }
-       }
- sg = use (myStyle.Default) $ do
-        
-}

type Vec = (Float,Float,Float)
type Name = String

data Module = Module

data Sg where 
    DrawIndexed  :: Int -> Sg
    Group        :: [Sg] -> Sg
    Transform    :: Vec -> Sg -> Sg
    Using        :: Module -> Sg -> Sg
    SetAttribute :: Attrib a => Name -> a -> Sg


data PrimitiveGeometry = Cube | Sphere

primitive :: PrimitiveGeometry -> Sg
primitive = undefined

testSg = Group [
                Transform (0,0,0) (DrawIndexed 10),
                Transform (10,10,10) (primitive Cube)
               ]

{-
data FootNMouth = FootNMouth
key = firstLabel FootNMouth "key"
name = nextLabel key "name"
breed = nextLabel name "breed"
price = nextLabel breed "price"
-}


unpricedAngus =     "jac" .=. (42::Integer)
                .*. emptyRecord

blubber = emptyRecord 
