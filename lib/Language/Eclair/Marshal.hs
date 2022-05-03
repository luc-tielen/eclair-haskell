{-# LANGUAGE TypeOperators, DefaultSignatures #-}

module Language.Eclair.Marshal
  ( Marshal(..)
  , GMarshal(..)
  , runMarshalM
  , MarshalM
  , valueSize
  ) where

import Control.Monad.State.Strict
import GHC.Generics
import Data.Int
import Foreign.Ptr
import Foreign.Storable
import qualified Language.Eclair.Internal.Constraints as C
import qualified Language.Eclair.Internal as Eclair


class Marshal a where
  serialize :: a -> MarshalM ()
  deserialize :: MarshalM a

  default serialize :: (Generic a, C.SimpleProduct a, GMarshal (Rep a)) => a -> MarshalM ()
  serialize a = gserialize (from a)

  default deserialize :: (Generic a, C.SimpleProduct a, GMarshal (Rep a)) => MarshalM a
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
