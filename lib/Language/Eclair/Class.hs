{-# LANGUAGE DataKinds, PolyKinds, TypeApplications, TypeOperators, TypeFamilies, UndecidableInstances #-}

module Language.Eclair.Class
  ( MonadEclair(..)
  , ContainsOutputFact
  , ContainsInputFact
  , ContainsFact
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
import Type.Errors.Pretty

type family ContainsOutputFact prog fact :: Constraint where
  ContainsOutputFact prog fact = (ContainsFact prog fact, IsOutput fact (FactDirection fact))

type family ContainsInputFact prog fact :: Constraint where
  ContainsInputFact prog fact = (ContainsFact prog fact, IsInput fact (FactDirection fact))

type family ContainsFact prog fact :: Constraint where
  ContainsFact prog fact =
    CheckContains prog (ProgramFacts prog) fact

type family CheckContains prog facts fact :: Constraint where
  CheckContains prog '[] fact =
    TypeError ("You tried to perform an action with a fact of type '" <> fact
    <> "' for program '" <> prog <> "'."
    % "The program contains the following facts: " <> ProgramFacts prog <> "."
    % "It does not contain fact: " <> fact <> "."
    % "You can fix this error by adding the type '" <> fact
    <> "' to the ProgramFacts type in the Program instance for " <> prog <> ".")
  CheckContains _ (a ': _) a = ()
  CheckContains prog (_ ': as) b = CheckContains prog as b

type family IsOutput (fact :: Type) (dir :: Direction) :: Constraint where
  IsOutput _ 'Output = ()
  IsOutput _ 'InputOutput = ()
  IsOutput fact dir = TypeError
    ( "You tried to use an " <> FormatDirection dir <> " fact of type " <> fact <> " as an output."
    % "Possible solution: change the FactDirection of " <> fact
      <> " to either 'Output' or 'InputOutput'."
    )

type family IsInput (fact :: Type) (dir :: Direction) :: Constraint where
  IsInput _ 'Input = ()
  IsInput _ 'InputOutput = ()
  IsInput fact dir = TypeError
    ( "You tried to use an " <> FormatDirection dir <> " fact of type " <> fact <> " as an input."
    % "Possible solution: change the FactDirection of " <> fact
      <> " to either 'Input' or 'InputOutput'."
    )

type family FormatDirection (dir :: Direction) where
  FormatDirection 'Output = "output"
  FormatDirection 'Input = "input"

class MonadEclair m where
  type Handler m :: Type -> Type

  addFacts :: forall prog f a. (Foldable f, Fact a, ContainsInputFact prog a, Sized (Rep a))
           => Handler m prog -> f a -> m ()

  addFact :: forall prog a. (Fact a, ContainsInputFact prog a, Sized (Rep a))
          => Handler m prog -> a -> m ()

  getFacts :: forall prog a. (Fact a, ContainsOutputFact prog a)
           => Handler m prog -> m [a]

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

  factType = const $ fromIntegral $ natVal (Proxy @ty)

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
