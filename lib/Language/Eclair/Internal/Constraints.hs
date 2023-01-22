{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Eclair.Internal.Constraints
  ( SimpleProduct
  , ProductLike
  , OnlySimpleFields
  , SimpleField
  ) where

import Data.Kind
import qualified Data.Text as T
import Data.Word
import GHC.Generics
import GHC.TypeLits

type family SimpleProduct (a :: Type) :: Constraint where
  SimpleProduct a = (ProductLike a (Rep a), OnlySimpleFields a (Rep a))

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

type family OnlySimpleFields (t :: Type) (f :: Type -> Type) :: Constraint where
  OnlySimpleFields t (a :*: b) = (OnlySimpleFields t a, OnlySimpleFields t b)
  OnlySimpleFields t (M1 _ _ a) = OnlySimpleFields t a
  OnlySimpleFields t (K1 _ a) = SimpleField t a
  OnlySimpleFields _ U1 = ()
  OnlySimpleFields _ V1 = ()

type family SimpleField (t :: Type) (a :: Type) :: Constraint where
  SimpleField _ Word32 = ()
  SimpleField _ T.Text = ()
  SimpleField t a =
    TypeError ('Text "Can only marshal values of type Text and Word32, but found '" ':<>: 'ShowType a ':<>: 'Text "' instead.")
