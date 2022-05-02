{-# LANGUAGE DataKinds, PolyKinds, TypeApplications, TypeOperators, TypeFamilies #-}

module Language.Eclair.Class
  ( MonadEclair(..)
  , Fact(..)
  , Direction(..)
  , Program(..)
  , Sized(..)
  , module Language.Eclair.Marshal
  , ProgramOptions(..)
  , FactOptions(..)
  ) where

import Data.Int
import Data.Kind
import Data.Proxy
import Data.Word
import GHC.Generics
import GHC.TypeLits
import Language.Eclair.Marshal


class MonadEclair m where
  type Handler m :: Type -> Type

  addFacts :: forall prog f a. (Foldable f, Fact a, Sized (Rep a)) => Handler m prog -> f a -> m ()

  addFact :: forall prog a. (Fact a, Sized (Rep a)) => Handler m prog -> a -> m ()

  getFacts :: forall prog a. Fact a => Handler m prog -> m [a]

  run :: Handler m prog -> m ()

data Direction
  = Input
  | Output
  | InputOutput

class Marshal a => Fact a where
  type FactDirection a :: Direction

  -- TODO: is there a way to remove this and become auto-generated?
  factType :: Proxy a -> Word16

newtype FactOptions a (dir :: Direction) (ty :: Nat)
  = FactOptions a

instance Marshal a => Marshal (FactOptions a dir ty) where
  serialize (FactOptions a) = serialize a
  deserialize = FactOptions <$> deserialize

instance (KnownNat ty, Marshal a) => Fact (FactOptions a dir ty) where
  type FactDirection (FactOptions _ dir _) = dir

  factType _ = fromIntegral $ natVal (Proxy @ty)

-- NOTE: this could be refactored into a type family right now, but later when
-- we have multiple eclair programs we will need a typeclass anyway
class Program a where
  type ProgramFacts a :: [Type]

newtype ProgramOptions (a :: Type) (facts :: [Type])
  = ProgramOptions a

instance Program (ProgramOptions a facts) where
  type ProgramFacts (ProgramOptions _ facts) = facts

class Sized (a :: k) where
  toSize :: Proxy a -> Int

instance Sized Int32 where
  toSize = const valueSize

instance Sized a => Sized (M1 i c a) where
  toSize = const $ toSize (Proxy @a)

instance Sized a => Sized (K1 i a) where
  toSize = const $ toSize (Proxy @a)

instance (Sized f, Sized g) => Sized (f :*: g) where
  toSize = const $
    toSize (Proxy @f) + toSize (Proxy @g)
