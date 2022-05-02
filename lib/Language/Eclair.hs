{-# LANGUAGE RoleAnnotations, DataKinds, TypeFamilies, DefaultSignatures #-}

module Language.Eclair
  ( module Language.Eclair  -- TODO refactor exports
  ) where

import Control.Monad.State.Strict
import Control.Monad.IO.Class
import GHC.Generics
import Data.Kind
import Data.Int
import Data.Foldable
import qualified Language.Eclair.Internal as Eclair
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable


-- TODO: add type error if:
-- not a product type
-- unsupported type in record (for now only allow Int32)
class Marshal a where
  serialize :: a -> MarshalM ()
  deserialize :: MarshalM a

  default serialize :: (Generic a, GMarshal (Rep a)) => a -> MarshalM ()
  serialize a = gserialize (from a)

  default deserialize :: (Generic a, GMarshal (Rep a)) => MarshalM a
  deserialize = to <$> gdeserialize

class GMarshal f where
  gserialize :: f a -> MarshalM ()
  gdeserialize :: MarshalM (f a)

newtype MarshalM a
  = MarshalM (StateT (Ptr Eclair.Buffer) IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadState (Ptr Eclair.Buffer))
  via StateT (Ptr Eclair.Buffer) IO

-- Every value in Eclair is 4 bytes
valueSize :: Int
valueSize = 4

writeAsBytes :: (Storable a, Marshal a) => a -> MarshalM ()
writeAsBytes a = do
  ptr <- gets castPtr
  liftIO $ poke ptr a
  put $ ptr `plusPtr` valueSize

readAsBytes :: (Storable a, Marshal a) => MarshalM a
readAsBytes = do
  ptr <- gets castPtr
  a <- liftIO $ peek ptr
  put $ ptr `plusPtr` valueSize
  pure a

instance Marshal Int32 where
  serialize = writeAsBytes
  deserialize = readAsBytes

addFacts = undefined

getFacts = undefined

data Direction
  = Input
  | Output
  | InputOutput

-- TODO: refactor into open type family?
class Fact a where
  type FactDirection a :: Direction

-- TODO: refactor into open type family?
class Program a where
  type ProgramFacts a :: [Type]

{-
@def edge(u32, u32).
@def path(u32, u32).

reachable(x, y) :-
  edge(x, y).

reachable(x, z) :-
  edge(x, y),
  reachable(y, z).
-}

-- The facts

data Edge
  = Edge Int32 Int32
  deriving (Generic)

data Reachable
  = Reachable Int32 Int32
  deriving (Show, Generic)

instance Fact Edge where
  type FactDirection Edge = 'Input

instance Fact Reachable where
  type FactDirection Reachable = 'Output

-- The program

data Path = Path

instance Program Path where
  type ProgramFacts Path = '[Edge, Reachable]

newtype Handle prog = Handle (Ptr Eclair.Program)
type role Handle nominal

newtype EclairM a
  = EclairM (IO a)
  deriving (Functor, Applicative, Monad)
  via IO

runEclairM :: EclairM a -> IO a
runEclairM (EclairM m) = m

withEclair :: Program prog => prog -> (Handle prog -> EclairM a) -> IO a
withEclair _prog f = do
  program <- Eclair.init
  withForeignPtr program $ runEclairM . f . Handle
run = undefined

exampleHighLevel :: IO ()
exampleHighLevel = do
  results <- withEclair Path $ \prog -> do
    addFacts prog [Edge 1 2, Edge 2 3]
    run prog
    getFacts prog
  process results
  where
    process :: [Reachable] -> IO ()
    process = traverse_ print



example :: Fact a => a -> IO ()
example facts = do
  program <- Eclair.init
  withForeignPtr program $ \prog -> do
    buffer <- mallocForeignPtrBytes _bytes  -- TODO: fill array
    withForeignPtr buffer $ \buf -> do
      Eclair.addFact prog _factType buf
      Eclair.addFacts prog _factType buf _count

    count <- Eclair.factCount prog _factType
    results <- Eclair.getFacts prog _factType
    withForeignPtr results $ \res -> do
      for_ [0..count] $ \i -> do
        -- TODO read from array
        _







-- TODO: addFact(s): need to create a buffer ourselves!
-- TODO: custom monad iso IO?
-- TODO use typesystem to avoid errors
-- TODO typeclasses for marshalling data
-- TODO derivingvia to hide all boilerplate
