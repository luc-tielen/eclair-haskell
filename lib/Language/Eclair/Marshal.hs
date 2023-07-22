{-# LANGUAGE DefaultSignatures, TypeOperators, TypeFamilies, DataKinds, UndecidableInstances #-}

module Language.Eclair.Marshal
  ( Marshal (..)
  , GMarshal (..)
  , runMarshalM
  , MarshalM
  , MarshalState (..)
  , valueSize
  ) where

import Control.Monad.State.Strict
import qualified Data.Text as T
import Data.Word
import Data.Kind
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics
import GHC.TypeLits
import qualified Language.Eclair.Internal as Eclair

class Marshal a where
  serialize :: a -> MarshalM ()
  deserialize :: MarshalM a

  default serialize :: (Generic a, SimpleProduct a, GMarshal (Rep a)) => a -> MarshalM ()
  serialize a = gserialize (from a)

  default deserialize :: (Generic a, SimpleProduct a, GMarshal (Rep a)) => MarshalM a
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

data MarshalState = MarshalState
  { programPtr :: Ptr Eclair.Program
  , bufPtr :: Ptr Eclair.Buffer
  }

newtype MarshalM a
  = MarshalM (StateT MarshalState IO a)
  deriving
    (Functor, Applicative, Monad, MonadIO, MonadState MarshalState)
    via StateT MarshalState IO

runMarshalM :: MarshalM a -> MarshalState -> IO a
runMarshalM (MarshalM m) =
  evalStateT m

-- Every value in Eclair is 4 bytes
valueSize :: Int
valueSize = 4

getBufPtr :: MarshalM (Ptr a)
getBufPtr =
  gets (castPtr . bufPtr)

updateBufPtr :: Ptr Eclair.Buffer -> MarshalM ()
updateBufPtr pointer =
  modify' $ \s -> s{bufPtr = pointer}

writeAsBytes :: (Storable a, Marshal a) => a -> MarshalM ()
writeAsBytes a = do
  ptr <- getBufPtr
  liftIO $ poke ptr a
  updateBufPtr (ptr `plusPtr` valueSize)

readAsBytes :: (Storable a, Marshal a) => MarshalM a
readAsBytes = do
  ptr <- getBufPtr
  a <- liftIO $ peek ptr
  updateBufPtr (ptr `plusPtr` valueSize)
  pure a

instance Marshal Word32 where
  serialize = writeAsBytes
  deserialize = readAsBytes

instance Marshal T.Text where
  serialize str = do
    prog <- gets programPtr
    index <- liftIO $ Eclair.encodeString prog str
    serialize index

  deserialize = do
    prog <- gets programPtr
    index <- deserialize
    liftIO $ Eclair.decodeString prog index

type family SimpleProduct (a :: Type) :: Constraint where
  SimpleProduct a = (ProductLike a (Rep a), OnlyMarshallableFields (Rep a))

type family ProductLike (t :: Type) (f :: Type -> Type) :: Constraint where
  ProductLike t (_ :*: b) = ProductLike t b
  ProductLike t (M1 _ _ a) = ProductLike t a
  ProductLike _ (K1 _ _) = ()
  ProductLike t (_ :+: _) =
    TypeError
      ( 'Text "Error while deriving marshalling code for type " ':<>: 'ShowType t ':<>: 'Text ":"
          ':$$: 'Text "Cannot derive sum type, only product types are supported."
      )
  ProductLike t U1 =
    TypeError
      ( 'Text "Error while deriving marshalling code for type " ':<>: 'ShowType t ':<>: 'Text ":"
          ':$$: 'Text "Cannot automatically derive code for 0 argument constructor."
      )
  ProductLike t V1 =
    TypeError
      ( 'Text "Error while deriving marshalling code for type " ':<>: 'ShowType t ':<>: 'Text ":"
          ':$$: 'Text "Cannot derive void type."
      )

type family OnlyMarshallableFields (f :: (Type -> Type)) where
  OnlyMarshallableFields (a :*: b) = (OnlyMarshallableFields a, OnlyMarshallableFields b)
  OnlyMarshallableFields (a :+: b) = (OnlyMarshallableFields a, OnlyMarshallableFields b)
  OnlyMarshallableFields (M1 _ _ a) = OnlyMarshallableFields a
  OnlyMarshallableFields U1 = ()
  OnlyMarshallableFields V1 = ()
  OnlyMarshallableFields k = OnlyMarshallableField k

type family OnlyMarshallableField (f :: (Type -> Type)) where
  OnlyMarshallableField (M1 _ _ a) = OnlyMarshallableField a
  OnlyMarshallableField (K1 _ a) = Marshal a
