{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators, UndecidableInstances #-}

module Language.Eclair.Internal.Constraints
  ( SimpleProduct
  , ProductLike
  , OnlyWord32Fields
  , Word32Field
  ) where

import Type.Errors.Pretty
import GHC.Generics
import Data.Kind
import Data.Word


type family SimpleProduct (a :: Type) :: Constraint where
  SimpleProduct a = (ProductLike a (Rep a), OnlyWord32Fields a (Rep a))

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

type family OnlyWord32Fields (t :: Type) (f :: Type -> Type) :: Constraint where
  OnlyWord32Fields t (a :*: b) = (OnlyWord32Fields t a, OnlyWord32Fields t b)
  OnlyWord32Fields t (M1 _ _ a) = OnlyWord32Fields t a
  OnlyWord32Fields t (K1 _ a) = Word32Field t a
  OnlyWord32Fields _ U1 = ()
  OnlyWord32Fields _ V1 = ()

type family Word32Field (t :: Type) (a :: Type) :: Constraint where
  Word32Field _ Word32 = ()
  Word32Field t a =
    TypeError ("Can only marshal values of Word32, but found '" <> a <> "' instead.")
