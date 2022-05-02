{-# LANGUAGE RoleAnnotations, DataKinds, TypeFamilies, DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, TypeOperators, PolyKinds #-}

module Language.Eclair
  ( module Language.Eclair  -- TODO refactor exports
  ) where

import Control.Monad.State.Strict
import GHC.Generics
import Data.Proxy
import Data.Kind
import Data.Int
import Data.Word
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

instance Marshal a => GMarshal (K1 i a) where
  gserialize (K1 x) = serialize x
  gdeserialize = K1 <$> deserialize

instance (GMarshal f, GMarshal g) => GMarshal (f :*: g) where
  gserialize (a :*: b) = do
    gserialize a
    gserialize b

  gdeserialize = (:*:) <$> gdeserialize <*> gdeserialize

instance GMarshal a => GMarshal (M1 i c a) where
  gserialize (M1 x) = gserialize x
  gdeserialize = M1 <$> gdeserialize

newtype MarshalM a
  = MarshalM (StateT (Ptr Eclair.Buffer) IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadState (Ptr Eclair.Buffer))
  via StateT (Ptr Eclair.Buffer) IO

runMarshalM :: MarshalM a -> Ptr Eclair.Buffer -> IO a
runMarshalM (MarshalM m) = evalStateT m

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

class ToByteSize (a :: k) where
  toByteSize :: Proxy a -> Int

instance ToByteSize Int32 where
  toByteSize = const valueSize

instance ToByteSize a => ToByteSize (M1 i c a) where
  toByteSize = const $ toByteSize (Proxy @a)

instance ToByteSize a => ToByteSize (K1 i a) where
  toByteSize = const $ toByteSize (Proxy @a)

instance (ToByteSize f, ToByteSize g) => ToByteSize (f :*: g) where
  toByteSize = const $
    toByteSize (Proxy @f) + toByteSize (Proxy @g)

-- TODO ContainsFact a
addFacts :: forall prog f a. (Foldable f, Fact a, ToByteSize (Rep a)) => Handle prog -> f a -> EclairM ()
addFacts (Handle prog) facts = EclairM $ do
  let factCount = length facts
      bytesPerFact = toByteSize (Proxy @(Rep a))
      byteCount = factCount * bytesPerFact
  buffer <- mallocForeignPtrBytes byteCount
  withForeignPtr buffer $ \buf -> do
    runMarshalM (traverse_ serialize facts) buf
    Eclair.addFacts prog (factType (Proxy @a)) buf (fromIntegral factCount)

-- TODO Fact a, ContainsFact a
-- TODO make polymorphic over container
getFacts :: forall prog a. Fact a => Handle prog -> EclairM [a]
getFacts (Handle prog) = EclairM $ do
  let ty = (factType (Proxy @a))
  count <- Eclair.factCount prog ty
  buffer <- Eclair.getFacts prog ty
  withForeignPtr buffer $ \buf ->
    runMarshalM (replicateM (fromIntegral count) deserialize) buf

run :: Handle prog -> EclairM ()
run (Handle prog) =
  EclairM $ Eclair.run prog

data Direction
  = Input
  | Output
  | InputOutput

class Marshal a => Fact a where
  type FactDirection a :: Direction

  -- TODO: is there a way to remove this and become auto-generated?
  factType :: Proxy a -> Word16

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
  deriving anyclass Marshal

data Reachable
  = Reachable Int32 Int32
  deriving (Show, Generic)
  deriving anyclass Marshal

instance Fact Edge where
  type FactDirection Edge = 'Input
  factType = const 0

instance Fact Reachable where
  type FactDirection Reachable = 'Output
  factType = const 1

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



-- example :: Fact a => a -> IO ()
-- example facts = do
--   program <- Eclair.init
--   withForeignPtr program $ \prog -> do
    -- buffer <- mallocForeignPtrBytes _bytes  -- TODO: fill array
    -- withForeignPtr buffer $ \buf -> do
    --   Eclair.addFact prog _factType buf
    --   Eclair.addFacts prog _factType buf _count

    -- count <- Eclair.factCount prog _factType
    -- results <- Eclair.getFacts prog _factType
    -- withForeignPtr results $ \res -> do
    --   for_ [0..count] $ \i -> do
    --     -- TODO read from array
    --     _


-- TODO: addFact(s): need to create a buffer ourselves!
-- TODO use typesystem to avoid errors
-- TODO derivingvia to hide all boilerplate
