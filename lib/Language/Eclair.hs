{-# LANGUAGE TypeFamilies, TypeApplications, RoleAnnotations, InstanceSigs #-}

module Language.Eclair
  ( EclairM
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

instance MonadEclair EclairM where
  type Handler EclairM = Handle

-- TODO ContainsFact a
  addFacts :: forall prog f a. (Foldable f, Fact a, Sized (Rep a)) => Handle prog -> f a -> EclairM ()
  addFacts (Handle prog) facts = EclairM $ do
    let factCount = length facts
        bytesPerFact = toSize (Proxy @(Rep a))
        byteCount = factCount * bytesPerFact
    buffer <- mallocForeignPtrBytes byteCount
    withForeignPtr buffer $ \buf -> do
      runMarshalM (traverse_ serialize facts) buf
      Internal.addFacts prog (factType (Proxy @a)) buf (fromIntegral factCount)

  addFact :: forall prog a. (Fact a, Sized (Rep a)) => Handle prog -> a -> EclairM ()
  addFact (Handle prog) fact = EclairM $ do
    let byteCount = toSize (Proxy @(Rep a))
    buffer <- mallocForeignPtrBytes byteCount
    withForeignPtr buffer $ \buf -> do
      runMarshalM (serialize fact) buf
      Internal.addFact prog (factType (Proxy @a)) buf

  -- TODO Fact a, ContainsFact a
  -- TODO make polymorphic over container
  getFacts :: forall prog a. Fact a => Handle prog -> EclairM [a]
  getFacts (Handle prog) = EclairM $ do
    let ty = (factType (Proxy @a))
    count <- Internal.factCount prog ty
    buffer <- Internal.getFacts prog ty
    withForeignPtr buffer $ \buf ->
      runMarshalM (replicateM (fromIntegral count) deserialize) buf

  run (Handle prog) =
    EclairM $ Internal.run prog

-- TODO use typesystem to avoid errors
-- TODO derivingvia to hide all boilerplate
