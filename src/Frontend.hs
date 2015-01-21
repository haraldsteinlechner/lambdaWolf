module Frontend where

import Data.Map(Map)
import qualified Data.Map as M
import Control.Monad
import Data.Monoid
import Control.Monad.Trans
import Control.Monad.Identity
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Text.Printf

-- [1] http://comonad.com/reader/2012/mirrored-lenses/

data Culling = CCW | CW deriving Show
data RasterizerState = RasterizerState 
                       { culling :: Culling -- ... 
                       } deriving Show

data BufferView = BufferView  deriving Show
type Semantic = String 

data Input = Input { bufferBinding :: Map Int BufferView, 
                     shaderParameters :: Map Semantic BufferView } deriving Show

emptyInput = Input M.empty M.empty

-- Graphics state is a product type of all pipeline states
data GS = GS { input :: Input, rasterizerState :: RasterizerState } deriving Show

emptyGS = GS emptyInput $ RasterizerState { culling = CW } 

data Framebuffer = Framebuffer -- persistent view onto possible mutable memory

-- Rendering is the process of taking a initial framebuffer and a thing
-- and produce a new framebuffer with a rendered into it.
type Rendering a = a -> Framebuffer -> Framebuffer 

-- Rendering takes place in an appropriate overloaded structure: State
-- monad. of course this thing now can also track all operations and build
-- dependency graphs et al.
type RenderM = StateT GS Identity

-- now renderM could also produce Renderings
type RenderingM a = Rendering (RenderM a)

-- PART I: public api which can be used to do low level hackery (much like
-- opengl)

-- we now define some actions which can be used for rendering
-- note that this setter stuff can be modelelled using lenses [for example
-- [1] which gives first class references.
-- however, for simplicity we won't do this now.

-- takes a cull mode and procues a action in RenderM which has no result
-- but an effect on the pi undefined
setCullMode :: Culling -> RenderM ()
setCullMode cullMode = modify setCull 
  where setCull (GS input r) = GS input $ r { culling = cullMode } -- nicer with lenses

type Index = Int -- Buffers are indexed by ints.

setBuffer :: BufferView -> Index -> RenderM ()
setBuffer buffer index = modify set
  where set s = let binding = input s -- very ugly here. but can be fixed ;)
                in s { input = binding { bufferBinding = setBuffer' binding } }
        setBuffer' = M.insert index buffer . bufferBinding 

-- Render lives in RenderM 
render :: RenderM ()
render = return ()

renderCube :: RenderM ()
renderCube = do
    setBuffer BufferView 0
    render

-- example: quasi opengl interface
myRendering :: RenderM ()
myRendering = do
    setCullMode CCW
    setBuffer BufferView 0
    render

-- higher order programming on functions with rendering effects
resetBuffers :: RenderM ()
resetBuffers = mapM_ (setBuffer BufferView) [1..16] -- resets all 16 buffer slots


-- PART II: imperative stuff should by reflective - we need to extract
-- dependency graphs.
data Log = LogSeq [Log] -- computation structure tree
         | Log String
         deriving Show

instance Monoid Log where
    mempty = LogSeq []
    (LogSeq a) `mappend` (LogSeq b) = LogSeq (a++b)
    l@(Log _)  `mappend` (LogSeq b) = LogSeq $ l:b
    (LogSeq a) `mappend` l@(Log _)  = LogSeq $ a ++ [l]
    Log s      `mappend` Log b      = LogSeq [Log s, Log b]

data Cube = Cube

type Scene = WriterT Log RenderM 

tellLog ::  Monad m => String -> WriterT Log m ()
tellLog = tell . Log 

cube :: Cube -> Scene ()
cube c = lift renderCube >> tellLog "renderCube\n"

data Transform = Transform

transform :: Transform -> Scene ()
transform _ = tellLog "transform\n"

transformScene :: Transform -> Scene () -> Scene ()
transformScene t g = transform t >> g

scene :: Scene ()
scene = transformScene Transform $ 
          replicateM_ 10 
            (transformScene Transform unitCube)
  where unitCube = cube Cube

runStructurue s = runIdentity $ (runStateT $ runWriterT s) startState
  where startState = emptyGS 

test :: WriterT Log Identity ()
test = tell (Log "abc") >> tell (Log "cdef")
