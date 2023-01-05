{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

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
    coerceTransformer,

    -- * Re-exports from generic-sop
    Generic,
    deriveGeneric,
  )
where

import Control.Arrow
import qualified Control.Category as Cat
import Data.Coerce (Coercible, coerce)
import Generics.SOP
import Generics.SOP.TH (deriveGeneric)
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

data Strategy = IdentityStrategy | OtherStrategy

type family StrategyFor a b where
  StrategyFor a a = 'IdentityStrategy
  StrategyFor a b = 'OtherStrategy

class Transformable' (strategy :: Strategy) a b where
  transformer' :: Proxy strategy -> Transformer a b

-- By having only one derived instance of @Transformable a b@, we can make it
-- overlappable without any errors.

instance
  {-# OVERLAPPABLE #-}
  (StrategyFor a b ~ strategy, Transformable' strategy a b) =>
  Transformable a b
  where
  transformer :: Transformer a b
  transformer = transformer' (Proxy :: Proxy strategy)

instance Transformable' 'IdentityStrategy a a where
  transformer' :: Proxy 'IdentityStrategy -> Transformer a a
  transformer' = const Cat.id

-- | A transformer that transforms each element of a traversable data structure.
-- If one transformation fails, this transformer fails. However, this is not
-- short-circuiting, so every 'IO' action will be run.
traversableTransformer ::
  (Traversable t, Transformable a b) => Transformer (t a) (t b)
traversableTransformer = Transformer $ fmap sequence . mapM transform

instance
  (Traversable t, Transformable a b) =>
  Transformable' 'OtherStrategy (t a) (t b)
  where
  transformer' :: Proxy 'OtherStrategy -> Transformer (t a) (t b)
  transformer' = const traversableTransformer

-- | A transformer that transforms each field of a term into the field of
-- another term with the same structure. If one transformation fails, this
-- transformer fails. However, this is not short-circuiting, so every 'IO'
-- action will be run.
genericTransformer ::
  (Generic a, Generic b, AllZip2 Transformable (Code a) (Code b)) =>
  Transformer a b
genericTransformer =
  Transformer $
    fmap (fmap to . hsequence)
      . hsequence'
      . htrans (Proxy :: Proxy Transformable) (Comp . transform . unI)
      . from

instance
  {-# OVERLAPPABLE #-}
  (Generic a, Generic b, AllZip2 Transformable (Code a) (Code b)) =>
  Transformable' 'OtherStrategy a b
  where
  transformer' :: Proxy 'OtherStrategy -> Transformer a b
  transformer' = const genericTransformer

-- | A transformer that is derived from a 'Coercible' instance. This transformer
-- does not fail.
coerceTransformer :: (Coercible a b) => Transformer a b
coerceTransformer = arr coerce
