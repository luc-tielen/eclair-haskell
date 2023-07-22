{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}

module Language.Eclair.Class
  ( MonadEclair (..)
  , ContainsOutputFact
  , ContainsInputFact
  , ContainsFact
  , Fact (..)
  , Direction (..)
  , Program (..)
  , Sized
  , ToSize(..)
  , GetFields
  , module Language.Eclair.Marshal
  , ProgramOptions (..)
  , FactOptions (..)
  ) where

import Control.Monad.Except
import Control.Monad.RWS
import Control.Monad.Reader
import qualified Control.Monad.State as Lazy
import qualified Control.Monad.State.Strict as Strict
import Control.Monad.Writer
import Data.Kind
import Data.Proxy
import Data.Text
import Data.Word
import GHC.Generics
import GHC.TypeLits
import Language.Eclair.Marshal

type family ContainsOutputFact prog fact :: Constraint where
  ContainsOutputFact prog fact = (ContainsFact prog fact, IsOutput fact (FactDirection fact))

type family ContainsInputFact prog fact :: Constraint where
  ContainsInputFact prog fact = (ContainsFact prog fact, IsInput fact (FactDirection fact))

type family ContainsFact prog fact :: Constraint where
  ContainsFact prog fact =
    CheckContains prog (ProgramFacts prog) fact

type family CheckContains prog facts fact :: Constraint where
  CheckContains prog '[] fact =
    TypeError
      ( 'Text "You tried to perform an action with a fact of type '"
          ':<>: 'ShowType fact
          ':<>: 'Text "' for program '"
          ':<>: 'ShowType prog
          ':<>: 'Text "'."
          ':$$: 'Text "The program contains the following facts: " ':<>: 'ShowType (ProgramFacts prog) ':<>: 'Text "."
          ':$$: 'Text "It does not contain fact: " ':<>: 'ShowType fact ':<>: 'Text "."
          ':$$: 'Text "You can fix this error by adding the type '"
            ':<>: 'ShowType fact
            ':<>: 'Text "' to the ProgramFacts type in the Program instance for "
            ':<>: 'ShowType prog
            ':<>: 'Text "."
      )
  CheckContains _ (a ': _) a = ()
  CheckContains prog (_ ': as) b = CheckContains prog as b

type family IsOutput (fact :: Type) (dir :: Direction) :: Constraint where
  IsOutput _ 'Output = ()
  IsOutput _ 'InputOutput = ()
  IsOutput fact dir =
    TypeError
      ( 'Text "You tried to use an "
          ':<>: 'ShowType (FormatDirection dir)
          ':<>: 'Text " fact of type "
          ':<>: 'ShowType fact
          ':<>: 'Text " as an output."
          ':$$: 'Text "Possible solution: change the FactDirection of "
            ':<>: 'ShowType fact
            ':<>: 'Text " to either 'Output' or 'InputOutput'."
      )

type family IsInput (fact :: Type) (dir :: Direction) :: Constraint where
  IsInput _ 'Input = ()
  IsInput _ 'InputOutput = ()
  IsInput fact dir =
    TypeError
      ( 'Text "You tried to use an "
          ':<>: 'ShowType (FormatDirection dir)
          ':<>: 'Text " fact of type "
          ':<>: 'ShowType fact
          ':<>: 'Text " as an input."
          ':$$: 'Text "Possible solution: change the FactDirection of "
            ':<>: 'ShowType fact
            ':<>: 'Text " to either 'Input' or 'InputOutput'."
      )

type family FormatDirection (dir :: Direction) where
  FormatDirection 'Output = "output"
  FormatDirection 'Input = "input"

data Direction
  = Input
  | Output
  | InputOutput

class Marshal a => Fact a where
  type FactDirection a :: Direction

  factName :: Proxy a -> Text

newtype FactOptions a (dir :: Direction) (name :: Symbol)
  = FactOptions a

instance Marshal a => Marshal (FactOptions a dir ty) where
  serialize (FactOptions a) = serialize a
  deserialize = FactOptions <$> deserialize

instance (KnownSymbol name, Marshal a) => Fact (FactOptions a dir name) where
  type FactDirection (FactOptions _ dir _) = dir

  factName = const $ pack $ symbolVal (Proxy @name)

-- NOTE: this could be refactored into a type family right now, but later when
-- we have multiple eclair programs we will need a typeclass anyway
class Program a where
  type ProgramFacts a :: [Type]

newtype ProgramOptions (a :: Type) (facts :: [Type])
  = ProgramOptions a

instance Program (ProgramOptions a facts) where
  type ProgramFacts (ProgramOptions _ facts) = facts

type Sized a = ToSize (GetFields (Rep a))

class ToSize (a :: k) where
  toSize :: Proxy a -> Int

instance ToSize Word32 where
  toSize = const valueSize
  {-# INLINEABLE toSize #-}

instance ToSize Text where
  toSize = const valueSize
  {-# INLINEABLE toSize #-}

instance ToSize a => ToSize (M1 i c a) where
  toSize = const $ toSize (Proxy @a)
  {-# INLINEABLE toSize #-}

instance ToSize a => ToSize (K1 i a) where
  toSize = const $ toSize (Proxy @a)
  {-# INLINEABLE toSize #-}

instance (ToSize f, ToSize g) => ToSize (f :*: g) where
  toSize = const $ toSize (Proxy @f) + toSize (Proxy @g)
  {-# INLINEABLE toSize #-}

instance ToSize '[] where
  toSize = const 0
  {-# INLINABLE toSize #-}

instance (ToSize a, ToSize as) => ToSize (a ': as) where
  toSize = const $ toSize (Proxy @a) + toSize (Proxy @as)
  {-# INLINABLE toSize #-}

-- | A helper type family, for getting all directly marshallable fields of a type.
type GetFields :: k -> [Type]
type family GetFields a where
  GetFields (K1 _ a) = DoGetFields a
  GetFields (M1 _ _ a) = GetFields a
  GetFields (f :*: g) = GetFields f ++ GetFields g

type DoGetFields :: Type -> [Type]
type family DoGetFields a where
  DoGetFields Word32 = '[Word32]
  DoGetFields Text = '[Text]
  DoGetFields a = GetFields (Rep a)

type (++) :: [Type] -> [Type] -> [Type]
type family a ++ b where
  '[] ++ b = b
  (a ': as) ++ bs = a ': as ++ bs

class Monad m => MonadEclair m where
  type Handler m :: Type -> Type
  type CollectFacts m :: (Type -> Type) -> Constraint

  addFacts
    :: forall prog f a
     . (Foldable f, Fact a, ContainsInputFact prog a, Sized a)
    => Handler m prog
    -> f a
    -> m ()

  addFact
    :: forall prog a
     . (Fact a, ContainsInputFact prog a, Sized a)
    => Handler m prog
    -> a
    -> m ()

  getFacts
    :: forall prog a c
     . (Fact a, ContainsOutputFact prog a, CollectFacts m c)
    => Handler m prog
    -> m (c a)

  run :: Handler m prog -> m ()

instance MonadEclair m => MonadEclair (ReaderT r m) where
  type Handler (ReaderT r m) = Handler m
  type CollectFacts (ReaderT r m) = CollectFacts m

  addFacts h = lift . addFacts h
  addFact h = lift . addFact h
  getFacts = lift . getFacts
  run = lift . run

instance (Monoid w, MonadEclair m) => MonadEclair (WriterT w m) where
  type Handler (WriterT w m) = Handler m
  type CollectFacts (WriterT w m) = CollectFacts m

  addFacts h = lift . addFacts h
  addFact h = lift . addFact h
  getFacts = lift . getFacts
  run = lift . run

instance MonadEclair m => MonadEclair (Strict.StateT s m) where
  type Handler (Strict.StateT s m) = Handler m
  type CollectFacts (Strict.StateT s m) = CollectFacts m

  addFacts h = lift . addFacts h
  addFact h = lift . addFact h
  getFacts = lift . getFacts
  run = lift . run

instance MonadEclair m => MonadEclair (Lazy.StateT s m) where
  type Handler (Lazy.StateT s m) = Handler m
  type CollectFacts (Lazy.StateT s m) = CollectFacts m

  addFacts h = lift . addFacts h
  addFact h = lift . addFact h
  getFacts = lift . getFacts
  run = lift . run

instance (MonadEclair m, Monoid w) => MonadEclair (RWST r w s m) where
  type Handler (RWST r w s m) = Handler m
  type CollectFacts (RWST r w s m) = CollectFacts m

  addFacts h = lift . addFacts h
  addFact h = lift . addFact h
  getFacts = lift . getFacts
  run = lift . run

instance MonadEclair m => MonadEclair (ExceptT e m) where
  type Handler (ExceptT e m) = Handler m
  type CollectFacts (ExceptT e m) = CollectFacts m

  addFacts h = lift . addFacts h
  addFact h = lift . addFact h
  getFacts = lift . getFacts
  run = lift . run
