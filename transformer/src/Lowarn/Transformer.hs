{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- |
-- Module                  : Lowarn.Transformer
-- SPDX-License-Identifier : MIT
-- Stability               : experimental
-- Portability             : non-portable (GHC)
--
-- Module for utilities for defining Lowarn state transformers.
module Lowarn.Transformer
  ( -- * Typeclass
    Transformable (..),

    -- * Transformers
    traversableTransformer,
    genericTransformer,
    genericTransformer',
    genericUnwrapTransformer,
    genericWrapTransformer,
    coerceTransformer,

    -- * Aliases
    DatatypeNameAlias,
    ConstructorNameAlias,

    -- * Re-exports from generic-sop
    Generic,
    deriveGeneric,
  )
where

import Control.Arrow
import qualified Control.Category as Cat
import Data.Coerce (Coercible, coerce)
import GHC.Base (Symbol)
import GHC.TypeLits (CmpSymbol)
import Generics.SOP
import Generics.SOP.Constraint
import Generics.SOP.TH (deriveGeneric)
import qualified Generics.SOP.Type.Metadata as M
import Lowarn (Transformer (..))

-- | A two-parameter typeclass that has instances for types @a@ and @b@ if a
-- @'Transformer' a b@ can be defined.
--
-- The purpose of this typeclass is to allow type-driven transformation of
-- state. This means that it can be used to generate transformers for updated
-- representations of state by only defining the constituent types that have
-- changed. This is achieved by a number of recursively-defined derived
-- instances on similar types. These instances can be overridden if necessary.
--
-- For every type @a@, there is an instance of @'Transformable' a a@.
--
-- For every pair of different types @t a@ and @t b@, where we have
-- @'Traversable' t@ and @'Transformable' a b@, there is an instance of
-- @'Transformable' (t a) (t b)@.
--
-- Finally, for every pair of different types @a@ and @b@ that are not of the
-- form @t c@ and @t d@ with @'Traversable' c d@, we may have an instance of
-- @'Transformable' a b@ using generic programming with @generics-sop@. We must
-- have both @'Generics.SOP.Generic' a@ and @'Generics.SOP.Generic' b@, as well
-- as @'Generics.SOP.AllZip2' 'Transformable' ('Generics.SOP.Code' a)
-- ('Generics.SOP.Code' b)@. This means that @a@ and @b@ must have the same
-- shape and that we require @'Transformable' c d@ for each field @c@ in @a@ and
-- corresponding field @d@ in @b@.
--
-- An example of using each of these instances, along with a manual instance, is
-- given below:
--
-- > {-# LANGUAGE DataKinds #-}
-- > {-# LANGUAGE InstanceSigs #-}
-- > {-# LANGUAGE MultiParamTypeClasses #-}
-- > {-# LANGUAGE TemplateHaskell #-}
-- > {-# LANGUAGE TypeFamilies #-}
-- >
-- > import Control.Arrow
-- > import Control.Monad
-- > import Data.Sequence (Seq)
-- > import Lowarn
-- > import Lowarn.Transformer
-- > import System.Environment (lookupEnv)
-- >
-- > newtype Variable = Variable
-- >   { name :: String
-- >   }
-- >
-- > data VariableWithValue = VariableWithValue
-- >   { key :: String,
-- >     value :: String
-- >   }
-- >
-- > data PreviousState a
-- >   = AccumulatedVariables Int a
-- >   | Variables (Seq Variable)
-- >
-- > data NextState a
-- >   = AccumulatedValues Int a
-- >   | VariablesWithValues (Seq VariableWithValue)
-- >
-- > deriveGeneric ''PreviousState
-- > deriveGeneric ''NextState
-- >
-- > instance Transformable Variable VariableWithValue where
-- >   transformer :: Transformer Variable VariableWithValue
-- >   transformer = arr VariableWithValue `ap` Transformer lookupEnv <<^ name
-- >
-- > instance DatatypeNameAlias "PreviousState" "NextState"
-- >
-- > instance ConstructorNameAlias "AccumulatedVariables" "AccumulatedValues"
-- >
-- > instance ConstructorNameAlias "Variables" "VariablesWithValues"
-- >
-- > customTransformer :: Transformer (PreviousState a) (NextState a)
-- > customTransformer = transformer
--
-- This typeclass is like 'Coercible', as it is used for converting between
-- types. However, this typeclass can be instantiated by the user. Another
-- difference is that the conversions defined by instances of this typeclass can
-- fail and have side-effects. 'coerceTransformer' can be used to instantiate
-- @'Transformable' a b@ given @'Coercible' a b@.
class Transformable a b where
  {-# MINIMAL transformer | transform #-}

  -- | Gives a transformer that attempts to transform data from type @a@ to type
  -- @b@.
  transformer :: Transformer a b
  transformer = Transformer transform

  -- | Attempt to transform data from type @a@ to type @b@, giving @Nothing@ if
  -- this is not possible.
  transform :: a -> IO (Maybe b)
  transform = unTransformer transformer

-- We can use a type family to stop @Transformable' a a@ overlapping with any
-- @Transformable' a b@. This is useful as we never want more than two instances
-- overlapping at once, and we have two derived instances of
-- @Transformable' a b@.

type family Equals a b :: Bool where
  Equals a a = 'True
  Equals a b = 'False

class Transformable' (isEqual :: Bool) a b where
  transformer' :: Proxy isEqual -> Transformer a b

-- By having only one derived instance of @Transformable a b@, we can make it
-- overlappable without any errors.

instance
  {-# OVERLAPPABLE #-}
  (isEqual ~ a `Equals` b, Transformable' isEqual a b) =>
  Transformable a b
  where
  transformer :: Transformer a b
  transformer = transformer' (Proxy :: Proxy isEqual)

instance Transformable' 'True a a where
  transformer' :: Proxy 'True -> Transformer a a
  transformer' = const Cat.id

-- | A transformer that transforms each element of a traversable data structure.
-- If one transformation fails, this transformer fails. However, this is not
-- short-circuiting, so every 'IO' action will be run.
traversableTransformer ::
  (Traversable t, Transformable a b) => Transformer (t a) (t b)
traversableTransformer = Transformer $ fmap sequence . mapM transform

instance
  (Traversable t, Transformable a b) =>
  Transformable' 'False (t a) (t b)
  where
  transformer' :: Proxy 'False -> Transformer (t a) (t b)
  transformer' = const traversableTransformer

genericTransform ::
  ( HTrans h1 h2,
    HSequence h2,
    SListIN h2 ys,
    SListIN (Prod h2) ys,
    AllZipN (Prod h1) Transformable xs ys
  ) =>
  h1 I xs ->
  IO (Maybe (h2 I ys))
genericTransform =
  fmap hsequence
    . hsequence'
    . htrans (Proxy :: Proxy Transformable) (Comp . transform . unI)

type TransformableCodes a b =
  (Generic a, Generic b, AllZip2 Transformable (Code a) (Code b))

-- | A transformer that transforms each field of a term into the field of
-- another term with the same structure. If one transformation fails, this
-- transformer fails. However, this is not short-circuiting, so every 'IO'
-- action will be run.
genericTransformer ::
  TransformableCodes a b =>
  Transformer a b
genericTransformer =
  Transformer $
    fmap (fmap to)
      . genericTransform
      . from

genericUnwrapTransformer ::
  (IsWrappedType a b', Transformable b' b) => Transformer a b
genericUnwrapTransformer = transformer <<^ wrappedTypeFrom

genericWrapTransformer ::
  (IsWrappedType b a', Transformable a a') => Transformer a b
genericWrapTransformer = wrappedTypeTo ^<< transformer

type family DatatypeNameOf (a :: M.DatatypeInfo) :: Symbol where
  DatatypeNameOf ('M.ADT _ datatypeName _ _) = datatypeName
  DatatypeNameOf ('M.Newtype _ datatypeName _) = datatypeName

type family
  ConstructorInfosOf (a :: M.DatatypeInfo) ::
    [M.ConstructorInfo]
  where
  DatatypeNameOf ('M.ADT _ _ constructorInfos _) = constructorInfos
  DatatypeNameOf ('M.Newtype _ _ constructorInfo) = '[constructorInfo]

type family ConstructorNameOf (a :: M.ConstructorInfo) :: Symbol where
  ConstructorNameOf ('M.Constructor constructorName) = constructorName
  ConstructorNameOf ('M.Infix constructorName _ _) = constructorName
  ConstructorNameOf ('M.Record constructorName _) = constructorName

type family SymbolEquals (a :: Symbol) (b :: Symbol) :: Constraint where
  SymbolEquals a b = (a `CmpSymbol` b) ~ 'EQ

class DatatypeNameAlias (a :: Symbol) (b :: Symbol)

instance {-# OVERLAPPABLE #-} SymbolEquals a b => DatatypeNameAlias a b

class ConstructorNameAlias (a :: Symbol) (b :: Symbol)

instance {-# OVERLAPPABLE #-} SymbolEquals a b => ConstructorNameAlias a b

type family ConstructorNamesOf (as :: [a]) :: [b] where
  ConstructorNamesOf '[] = '[]
  ConstructorNamesOf (x ': xs) = ConstructorNameOf x ': ConstructorNamesOf xs

class DatatypesMatch a b

instance
  ( HasDatatypeInfo a,
    HasDatatypeInfo b,
    da ~ DatatypeInfoOf a,
    db ~ DatatypeInfoOf b,
    DatatypeNameAlias (DatatypeNameOf da) (DatatypeNameOf db),
    cas ~ ConstructorInfosOf da,
    cbs ~ ConstructorInfosOf db,
    AllZip
      ConstructorNameAlias
      (ConstructorNamesOf cas)
      (ConstructorNamesOf cbs)
  ) =>
  DatatypesMatch a b

genericTransformer' ::
  (TransformableCodes a b, DatatypesMatch a b) =>
  Transformer a b
genericTransformer' = genericTransformer

instance
  {-# OVERLAPPABLE #-}
  (TransformableCodes a b, DatatypesMatch a b) =>
  Transformable' 'False a b
  where
  transformer' :: Proxy 'False -> Transformer a b
  transformer' = const genericTransformer'

-- | A transformer that is derived from a 'Coercible' instance. This transformer
-- does not fail.
coerceTransformer :: (Coercible a b) => Transformer a b
coerceTransformer = arr coerce
