{-# LANGUAGE TypeFamilies, TypeApplications, RoleAnnotations, InstanceSigs #-}

module Language.Eclair
  ( module Language.Eclair.Class
  , EclairM
  , Handle
  , withEclair
  ) where

import Language.Eclair.Class
import Control.Monad.State.Strict
import GHC.Generics
import Data.Proxy
import Data.Foldable
import qualified Language.Eclair.Internal as Internal
import Foreign.ForeignPtr
import Foreign.Ptr


newtype Handle prog = Handle (Ptr Internal.Program)
type role Handle nominal

newtype EclairM a
  = EclairM { runEclairM :: IO a }
  deriving (Functor, Applicative, Monad)
  via IO

-- NOTE: program variable is currently only used on the type level
withEclair :: Program prog => prog -> (Handle prog -> EclairM a) -> IO a
withEclair _prog f = do
  program <- Internal.init
  withForeignPtr program $ runEclairM . f . Handle

class Collect c where
  collect :: Marshal a => Int -> MarshalM (c a)

instance Collect [] where
  collect count =
    replicateM count deserialize

instance MonadEclair EclairM where
  type Handler EclairM = Handle
  type CollectFacts EclairM = Collect

  addFacts :: forall prog f a. (Foldable f, Fact a, ContainsInputFact prog a, Sized (Rep a))
           => Handle prog -> f a -> EclairM ()
  addFacts (Handle prog) facts = EclairM $ do
    let factCount = length facts
        bytesPerFact = toSize (Proxy @(Rep a))
        byteCount = factCount * bytesPerFact
    buffer <- mallocForeignPtrBytes byteCount
    withForeignPtr buffer $ \buf -> do
      runMarshalM (traverse_ serialize facts) buf
      Internal.addFacts prog (factType (Proxy @a)) buf (fromIntegral factCount)

  addFact :: forall prog a. (Fact a, ContainsInputFact prog a, Sized (Rep a))
          => Handle prog -> a -> EclairM ()
  addFact (Handle prog) fact = EclairM $ do
    let byteCount = toSize (Proxy @(Rep a))
    buffer <- mallocForeignPtrBytes byteCount
    withForeignPtr buffer $ \buf -> do
      runMarshalM (serialize fact) buf
      Internal.addFact prog (factType (Proxy @a)) buf

  getFacts :: forall prog a c. (Fact a, ContainsOutputFact prog a, Collect c)
           => Handle prog -> EclairM (c a)
  getFacts (Handle prog) = EclairM $ do
    let ty = (factType (Proxy @a))
    count <- Internal.factCount prog ty
    buffer <- Internal.getFacts prog ty
    withForeignPtr buffer $ \buf ->
      runMarshalM (collect (fromIntegral count)) buf

  run (Handle prog) =
    EclairM $ Internal.run prog
