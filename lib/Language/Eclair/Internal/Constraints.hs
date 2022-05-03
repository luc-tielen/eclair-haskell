{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators, UndecidableInstances #-}

module Language.Eclair.Internal.Constraints
  ( SimpleProduct
  , ProductLike
  , OnlyInt32Fields
  , Int32Field
  ) where

import Type.Errors.Pretty
import GHC.Generics
import Data.Kind
import Data.Int


type family SimpleProduct (a :: Type) :: Constraint where
  SimpleProduct a = (ProductLike a (Rep a), OnlyInt32Fields a (Rep a))

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

type family OnlyInt32Fields (t :: Type) (f :: Type -> Type) :: Constraint where
  OnlyInt32Fields t (a :*: b) = (OnlyInt32Fields t a, OnlyInt32Fields t b)
  OnlyInt32Fields t (M1 _ _ a) = OnlyInt32Fields t a
  OnlyInt32Fields t (K1 _ a) = Int32Field t a
  OnlyInt32Fields _ U1 = ()
  OnlyInt32Fields _ V1 = ()

type family Int32Field (t :: Type) (a :: Type) :: Constraint where
  Int32Field _ Int32 = ()
  Int32Field t a =
    TypeError ("Can only marshal values of Int32, but found '" <> a <> "' instead.")
