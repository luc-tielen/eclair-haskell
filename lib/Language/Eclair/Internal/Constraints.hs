{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators, UndecidableInstances #-}

module Language.Eclair.Internal.Constraints
  ( SimpleProduct
  , ProductLike
  , OnlySimpleFields
  , SimpleField
  ) where

import Type.Errors.Pretty
import GHC.Generics
import Data.Kind
import Data.Word
import qualified Data.Text as T


type family SimpleProduct (a :: Type) :: Constraint where
  SimpleProduct a = (ProductLike a (Rep a), OnlySimpleFields a (Rep a))

type family ProductLike (t :: Type) (f :: Type -> Type) :: Constraint where
  ProductLike t (_ :*: b) = ProductLike t b
  ProductLike t (M1 _ _ a) = ProductLike t a
  ProductLike _ (K1 _ _) = ()
  ProductLike t (_ :+: _) =
    TypeError ( "Error while deriving marshalling code for type " <> t <> ":"
              % "Cannot derive sum type, only product types are supported.")
  ProductLike t U1 =
    TypeError ( "Error while deriving marshalling code for type " <> t <> ":"
              % "Cannot automatically derive code for 0 argument constructor.")
  ProductLike t V1 =
    TypeError ( "Error while deriving marshalling code for type " <> t <> ":"
              % "Cannot derive void type.")

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
    TypeError ("Can only marshal values of type Text and Word32, but found '" <> a <> "' instead.")
