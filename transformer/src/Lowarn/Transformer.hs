{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
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

    -- ** Default transformers
    traversableTransformer,
    genericRenamingTransformer,

    -- ** Additional generic transformers
    genericTransformer,
    genericReorderingTransformer,
    genericUnwrapTransformer,
    genericWrapTransformer,

    -- ** Coercion transformers
    coerceTransformer,

    -- * Aliases
    DatatypeNameAlias,
    ConstructorNameAlias,
    FieldNameAlias,

    -- * Re-exports from generic-sop
    Generic,
    deriveGeneric,
  )
where

import Control.Arrow
import qualified Control.Category as Cat
import Data.Coerce (Coercible, coerce)
import Data.Kind (Type)
import GHC.TypeLits (CmpSymbol, Symbol)
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

class Transformable' (isEqual :: Bool) a b where
  transformer' :: Proxy isEqual -> Transformer a b

-- By having only one derived instance of @Transformable a b@, we can make it
-- overlappable without any errors.

type family Equals a b :: Bool where
  Equals a a = 'True
  Equals a b = 'False

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

type TransformableCodes a b =
  (Generic a, Generic b, AllZip2 Transformable (Code a) (Code b))

type TransformableCodes' as bs =
  (SListIN (Prod SOP) bs, AllZip2 Transformable as bs)

-- | A transformer that transforms each field of a term into the field of
-- another term with the same structure. If one transformation fails, this
-- transformer fails. However, this is not short-circuiting, so every 'IO'
-- action will be run.
genericTransformer ::
  TransformableCodes a b =>
  Transformer a b
genericTransformer = Transformer genericTransform

genericTransform ::
  TransformableCodes a b =>
  a ->
  IO (Maybe b)
genericTransform = fmap (fmap to) . genericTransform' . from

genericTransform' ::
  TransformableCodes' as bs =>
  SOP I as ->
  IO (Maybe (SOP I bs))
genericTransform' =
  fmap hsequence
    . hsequence'
    . htrans (Proxy :: Proxy Transformable) (Comp . transform . unI)

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

type family ConstructorNamesOf (a :: [M.ConstructorInfo]) :: [Symbol] where
  ConstructorNamesOf '[] = '[]
  ConstructorNameOf (c ': cs) = ConstructorNameOf c ': ConstructorNamesOf cs

type family
  FieldInfosOf (a :: M.ConstructorInfo) ::
    [M.FieldInfo]
  where
  FieldInfosOf ('M.Constructor _) = '[]
  FieldInfosOf ('M.Infix _ _ _) = '[]
  FieldInfosOf ('M.Record _ fieldInfos) = fieldInfos

type family FieldNameOf (a :: M.FieldInfo) :: Symbol where
  FieldNameOf ('M.FieldInfo fieldName) = fieldName

type family FieldNamesOf (a :: [M.FieldInfo]) :: [Symbol] where
  FieldNamesOf '[] = '[]
  FieldNamesOf (f ': fs) = FieldNameOf f ': FieldNamesOf fs

type family
  FieldNamesOfConstructors
    (a :: [M.ConstructorInfo]) ::
    [[Symbol]]
  where
  FieldNamesOfConstructors '[] = '[]
  FieldNamesOfConstructors (c ': cs) =
    FieldNamesOf (FieldInfosOf c) ': FieldNamesOfConstructors cs

type family SymbolEquals (a :: Symbol) (b :: Symbol) :: Constraint where
  SymbolEquals a b = (a `CmpSymbol` b) ~ 'EQ

type family OrderingIsEq (a :: Ordering) :: Bool where
  OrderingIsEq 'EQ = 'True
  OrderingIsEq 'LT = 'False
  OrderingIsEq 'GT = 'False

type family SymbolEqualsBool (a :: Symbol) (b :: Symbol) :: Bool where
  SymbolEquals a b = OrderingIsEq (a `CmpSymbol` b)

class DatatypeNameAlias (a :: Symbol) (b :: Symbol)

instance {-# OVERLAPPABLE #-} SymbolEquals a b => DatatypeNameAlias a b

class ConstructorNameAlias (a :: Symbol) (b :: Symbol)

instance {-# OVERLAPPABLE #-} SymbolEquals a b => ConstructorNameAlias a b

class FieldNameAlias (a :: Symbol) (b :: Symbol)

instance {-# OVERLAPPABLE #-} SymbolEquals a b => FieldNameAlias a b

type DatatypesMatchRenamingConstraint a b =
  ( HasDatatypeInfo a,
    HasDatatypeInfo b,
    DatatypeNameAlias
      (DatatypeNameOf (DatatypeInfoOf a))
      (DatatypeNameOf (DatatypeInfoOf b)),
    AllZip
      ConstructorsMatch
      (ConstructorInfosOf (DatatypeInfoOf a))
      (ConstructorInfosOf (DatatypeInfoOf b))
  )

class DatatypesMatchRenamingConstraint a b => DatatypesMatchRenaming a b

instance DatatypesMatchRenamingConstraint a b => DatatypesMatchRenaming a b

type ConstructorsMatchConstraint a b =
  ( ConstructorNameAlias (ConstructorNameOf a) (ConstructorNameOf b),
    AllZip FieldsMatch (FieldInfosOf a) (FieldInfosOf b)
  )

class ConstructorsMatchConstraint a b => ConstructorsMatch a b

instance ConstructorsMatchConstraint a b => ConstructorsMatch a b

type FieldsMatchConstraint a b =
  (FieldNameAlias (FieldNameOf a) (FieldNameOf b))

class FieldsMatchConstraint a b => FieldsMatch a b

instance FieldsMatchConstraint a b => FieldsMatch a b

genericRenamingTransformer ::
  (TransformableCodes a b, DatatypesMatchRenaming a b) =>
  Transformer a b
genericRenamingTransformer = genericTransformer

instance
  {-# OVERLAPPABLE #-}
  ( TransformableCodes a b,
    DatatypesMatchRenaming a b
  ) =>
  Transformable' 'False a b
  where
  transformer' :: Proxy 'False -> Transformer a b
  transformer' = const genericRenamingTransformer

-- Reordering

genericReorderingTransformer ::
  forall a b was cs wcs scss ds.
  ( TransformableCodes' ds (Code b),
    DatatypesMatchReordering a b was cs wcs scss ds
  ) =>
  Transformer a b
genericReorderingTransformer =
  Transformer $
    fmap (fmap to)
      . genericTransform'
      . (reorderConstructors @a @b @was @cs @wcs @scss @ds)
      . from

class
  ( HasDatatypeInfo a,
    HasDatatypeInfo b,
    DatatypeNameAlias
      (DatatypeNameOf (DatatypeInfoOf a))
      (DatatypeNameOf (DatatypeInfoOf b)),
    was
      ~ ZipSymbolsWithSymbols
          (ConstructorNamesOf (ConstructorInfosOf (DatatypeInfoOf a)))
          (FieldNamesOfConstructors (ConstructorInfosOf (DatatypeInfoOf a))),
    OrderWithSymbolsNS
      (Code a)
      was
      (ConstructorNamesOf (ConstructorInfosOf (DatatypeInfoOf b)))
      cs
      wcs,
    scss ~ WithSymbols wcs,
    OrderWithSymbolsNPs
      cs
      scss
      (FieldNamesOfConstructors (ConstructorInfosOf (DatatypeInfoOf b)))
      ds
  ) =>
  DatatypesMatchReordering a b was cs wcs scss ds
    | a b -> was wcs cs wcs scss ds
  where
  reorderConstructors :: SOP f (Code a) -> SOP f ds

instance
  forall
    (a :: Type)
    (b :: Type)
    (was :: [SymbolWithSymbols])
    (cs :: [[Type]])
    (wcs :: [SymbolWithSymbols])
    (scss :: [[Symbol]])
    (ds :: [[Type]]).
  ( HasDatatypeInfo a,
    HasDatatypeInfo b,
    DatatypeNameAlias
      (DatatypeNameOf (DatatypeInfoOf a))
      (DatatypeNameOf (DatatypeInfoOf b)),
    was
      ~ ZipSymbolsWithSymbols
          (ConstructorNamesOf (ConstructorInfosOf (DatatypeInfoOf a)))
          (FieldNamesOfConstructors (ConstructorInfosOf (DatatypeInfoOf a))),
    OrderWithSymbolsNS
      (Code a)
      was
      (ConstructorNamesOf (ConstructorInfosOf (DatatypeInfoOf b)))
      cs
      wcs,
    scss ~ WithSymbols wcs,
    OrderWithSymbolsNPs
      cs
      scss
      (FieldNamesOfConstructors (ConstructorInfosOf (DatatypeInfoOf b)))
      ds
  ) =>
  DatatypesMatchReordering a b was cs wcs scss ds
  where
  reorderConstructors :: forall (f :: Type -> Type). SOP f (Code a) -> SOP f ds
  reorderConstructors sop =
    SOP $
      orderNPs
        @Type
        @cs
        @scss
        @(FieldNamesOfConstructors (ConstructorInfosOf (DatatypeInfoOf b)))
        zs
    where
      zs :: NS (NP f) cs
      zs =
        orderNS
          @[Type]
          @(Code a)
          @was
          @(ConstructorNamesOf (ConstructorInfosOf (DatatypeInfoOf b)))
          (unSOP sop)

data SymbolWithSymbols = SymbolWithSymbols
  { _symbol :: Symbol,
    _withSymbols :: [Symbol]
  }

type family Symbols (a :: [SymbolWithSymbols]) :: [Symbol] where
  Symbols '[] = '[]
  Symbols ('SymbolWithSymbols symbol withSymbols ': as) =
    symbol ': Symbols as

type family WithSymbols (a :: [SymbolWithSymbols]) :: [[Symbol]] where
  WithSymbols '[] = '[]
  WithSymbols ('SymbolWithSymbols symbol withSymbols ': as) =
    withSymbols ': WithSymbols as

type family
  ZipSymbolsWithSymbols (a :: [Symbol]) (b :: [[Symbol]]) ::
    [SymbolWithSymbols]
  where
  ZipSymbolsWithSymbols '[] '[] = '[]
  ZipSymbolsWithSymbols (symbol ': symbols) (withSymbols ': withSymbolss) =
    'SymbolWithSymbols symbol withSymbols
      ': ZipSymbolsWithSymbols symbols withSymbolss

type family ZipSymbolsWithNoSymbols (a :: [Symbol]) :: [SymbolWithSymbols] where
  ZipSymbolsWithNoSymbols '[] = '[]
  ZipSymbolsWithNoSymbols (symbol ': symbols) =
    'SymbolWithSymbols symbol '[] ': ZipSymbolsWithNoSymbols symbols

class
  ( b ~ TakeFromSymbolsB as was s,
    wb ~ TakeFromSymbolsWb as was s,
    cs ~ TakeFromSymbolsCs as was s,
    wcs ~ TakeFromSymbolsWcs as was s,
    as ~ (Head as ': Tail as),
    was ~ (Head was ': Tail was)
  ) =>
  TakeWithSymbols
    (as :: [k])
    (was :: [SymbolWithSymbols])
    (s :: Symbol)
    (b :: k)
    (wb :: SymbolWithSymbols)
    (cs :: [k])
    (wcs :: [SymbolWithSymbols])
  where
  type TakeFromSymbolsB as was s :: k
  type TakeFromSymbolsWb as was s :: SymbolWithSymbols
  type TakeFromSymbolsCs as was s :: [k]
  type TakeFromSymbolsWcs as was s :: [SymbolWithSymbols]

  takeWithSymbols :: NP f as -> (f b, NP f cs)

class
  ( b ~ TakeFromSymbols'B p as was s,
    wb ~ TakeFromSymbols'Wb p as was s,
    cs ~ TakeFromSymbols'Cs p as was s,
    wcs ~ TakeFromSymbols'Wcs p as was s,
    as ~ (Head as ': Tail as),
    was ~ (Head was ': Tail was)
  ) =>
  TakeWithSymbols'
    (p :: Bool)
    (as :: [k])
    (was :: [SymbolWithSymbols])
    (s :: Symbol)
    (b :: k)
    (wb :: SymbolWithSymbols)
    (cs :: [k])
    (wcs :: [SymbolWithSymbols])
  where
  type TakeFromSymbols'B p as was s :: k
  type TakeFromSymbols'Wb p as was s :: SymbolWithSymbols
  type TakeFromSymbols'Cs p as was s :: [k]
  type TakeFromSymbols'Wcs p as was s :: [SymbolWithSymbols]

  takeWithSymbols' :: NP f as -> (f b, NP f cs)

instance
  forall
    k
    (p :: Bool)
    (a :: k)
    (as :: [k])
    (sa :: Symbol)
    (sa' :: [Symbol])
    (was :: [SymbolWithSymbols])
    (s :: Symbol)
    (b :: k)
    (wb :: SymbolWithSymbols)
    (cs :: [k])
    (wcs :: [SymbolWithSymbols]).
  ( p ~ sa `SymbolEqualsBool` s,
    TakeWithSymbols'
      p
      (a ': as)
      ('SymbolWithSymbols sa sa' ': was)
      s
      b
      wb
      cs
      wcs
  ) =>
  TakeWithSymbols (a ': as) ('SymbolWithSymbols sa sa' ': was) s b wb cs wcs
  where
  type
    TakeFromSymbolsB (a ': as) ('SymbolWithSymbols sa sa' ': was) s =
      TakeFromSymbols'B
        (sa `SymbolEqualsBool` s)
        (a ': as)
        ('SymbolWithSymbols sa sa' ': was)
        s

  type
    TakeFromSymbolsWb (a ': as) ('SymbolWithSymbols sa sa' ': was) s =
      TakeFromSymbols'Wb
        (sa `SymbolEqualsBool` s)
        (a ': as)
        ('SymbolWithSymbols sa sa' ': was)
        s

  type
    TakeFromSymbolsCs (a ': as) ('SymbolWithSymbols sa sa' ': was) s =
      TakeFromSymbols'Cs
        (sa `SymbolEqualsBool` s)
        (a ': as)
        ('SymbolWithSymbols sa sa' ': was)
        s

  type
    TakeFromSymbolsWcs (a ': as) ('SymbolWithSymbols sa sa' ': was) s =
      TakeFromSymbols'Wcs
        (sa `SymbolEqualsBool` s)
        (a ': as)
        ('SymbolWithSymbols sa sa' ': was)
        s

  takeWithSymbols =
    takeWithSymbols' @k @p @(a ': as) @('SymbolWithSymbols sa sa' ': was) @s

instance TakeWithSymbols' 'True (a ': as) (wa ': was) s a wa as was where
  type TakeFromSymbols'B 'True (a ': as) (wa ': was) s = a
  type TakeFromSymbols'Wb 'True (a ': as) (wa ': was) s = wa
  type TakeFromSymbols'Cs 'True (a ': as) (wa ': was) s = as
  type TakeFromSymbols'Wcs 'True (a ': as) (wa ': was) s = was

  takeWithSymbols' :: NP f (a ': as) -> (f a, NP f as)
  takeWithSymbols' (x :* xs) = (x, xs)

instance
  forall
    k
    (a1 :: k)
    (a2 :: k)
    (as :: [k])
    (wa1 :: SymbolWithSymbols)
    (wa2 :: SymbolWithSymbols)
    (was :: [SymbolWithSymbols])
    (s :: Symbol)
    (b :: k)
    (wb :: SymbolWithSymbols)
    (cs :: [k])
    (wcs :: [SymbolWithSymbols]).
  ( TakeWithSymbols (a2 ': as) (wa2 ': was) s b wb cs wcs
  ) =>
  TakeWithSymbols'
    'False
    (a1 ': a2 ': as)
    (wa1 ': wa2 ': was)
    s
    b
    wb
    (a1 ': cs)
    (wa1 ': wcs)
  where
  type
    TakeFromSymbols'B 'False (a1 ': a2 ': as) (wa1 ': wa2 ': was) s =
      TakeFromSymbolsB (a2 ': as) (wa2 ': was) s

  type
    TakeFromSymbols'Wb 'False (a1 ': a2 ': as) (wa1 ': wa2 ': was) s =
      TakeFromSymbolsWb (a2 ': as) (wa2 ': was) s

  type
    TakeFromSymbols'Cs 'False (a1 ': a2 ': as) (wa1 ': wa2 ': was) s =
      (a1 ': TakeFromSymbolsCs (a2 ': as) (wa2 ': was) s)

  type
    TakeFromSymbols'Wcs 'False (a1 ': a2 ': as) (wa1 ': wa2 ': was) s =
      (wa1 ': TakeFromSymbolsWcs (a2 ': as) (wa2 ': was) s)

  takeWithSymbols' ::
    forall (f :: k -> Type).
    NP f (a1 ': a2 ': as) ->
    (f b, NP f (a1 ': cs))
  takeWithSymbols' (x1 :* x2 :* xs) = (y, x1 :* zs)
    where
      y :: f b
      zs :: NP f cs
      (y, zs) = takeWithSymbols @k @(a2 ': as) @(wa2 ': was) @s (x2 :* xs)

class
  ( bs ~ OrderWithSymbolsBs as was ss,
    wbs ~ OrderWithSymbolsWbs as was ss
  ) =>
  OrderWithSymbols
    (as :: [k])
    (was :: [SymbolWithSymbols])
    (ss :: [Symbol])
    (bs :: [k])
    (wbs :: [SymbolWithSymbols])
  where
  type OrderWithSymbolsBs as was ss :: [k]
  type OrderWithSymbolsWbs as was ss :: [SymbolWithSymbols]

  orderNP :: NP f as -> NP f bs

instance OrderWithSymbols '[] '[] '[] '[] '[] where
  type OrderWithSymbolsBs '[] '[] '[] = '[]
  type OrderWithSymbolsWbs '[] '[] '[] = '[]

  orderNP :: NP f '[] -> NP f '[]
  orderNP Nil = Nil

instance
  forall
    k
    (a :: k)
    (as :: [k])
    (wa :: SymbolWithSymbols)
    (was :: [SymbolWithSymbols])
    (s :: Symbol)
    (ss :: [Symbol])
    (b :: k)
    (bs :: [k])
    (wb :: SymbolWithSymbols)
    (wbs :: [SymbolWithSymbols])
    (ds :: [k])
    (wds :: [SymbolWithSymbols]).
  ( TakeWithSymbols (a ': as) (wa ': was) s b wb ds wds,
    OrderWithSymbols ds wds ss bs wbs
  ) =>
  OrderWithSymbols (a ': as) (wa ': was) (s ': ss) (b ': bs) (wb ': wbs)
  where
  type
    OrderWithSymbolsBs (a ': as) (wa ': was) (s ': ss) =
      TakeFromSymbolsB (a ': as) (wa ': was) s
        ': OrderWithSymbolsBs
             (TakeFromSymbolsCs (a ': as) (wa ': was) s)
             (TakeFromSymbolsWcs (a ': as) (wa ': was) s)
             ss
  type
    OrderWithSymbolsWbs (a ': as) (wa ': was) (s ': ss) =
      TakeFromSymbolsWb (a ': as) (wa ': was) s
        ': OrderWithSymbolsWbs
             (TakeFromSymbolsCs (a ': as) (wa ': was) s)
             (TakeFromSymbolsWcs (a ': as) (wa ': was) s)
             ss

  orderNP ::
    forall (f :: k -> Type).
    NP f (a ': as) ->
    NP f (b ': bs)
  orderNP (x :* xs) =
    y :* orderNP @k @ds @wds @ss ws
    where
      y :: f b
      ws :: NP f ds
      (y, ws) = takeWithSymbols @k @(a ': as) @(wa ': was) @s (x :* xs)

class
  ( bs ~ OrderWithSymbolsNSBs as was ss,
    wbs ~ OrderWithSymbolsNSWbs as was ss,
    OrderWithSymbols as was ss bs wbs,
    SListI as,
    SListI bs
  ) =>
  OrderWithSymbolsNS
    (as :: [k])
    (was :: [SymbolWithSymbols])
    (ss :: [Symbol])
    (bs :: [k])
    (wbs :: [SymbolWithSymbols])
  where
  type OrderWithSymbolsNSBs as was ss :: [k]
  type OrderWithSymbolsNSWbs as was ss :: [SymbolWithSymbols]

  orderNS :: NS f as -> NS f bs

instance
  forall
    k
    (a :: k)
    (as :: [k])
    (wa :: SymbolWithSymbols)
    (was :: [SymbolWithSymbols])
    (s :: Symbol)
    (ss :: [Symbol])
    (b :: k)
    (bs :: [k])
    (wb :: SymbolWithSymbols)
    (wbs :: [SymbolWithSymbols])
    (sas :: [Symbol]).
  ( OrderWithSymbols (a ': as) (wa ': was) (s ': ss) (b ': bs) (wb ': wbs),
    sas ~ Symbols (wa ': was),
    OrderWithSymbols
      (b ': bs)
      (wb ': wbs)
      sas
      (a ': as)
      (wa ': was),
    SListI (a ': as),
    SListI (b ': bs)
  ) =>
  OrderWithSymbolsNS (a ': as) (wa ': was) (s ': ss) (b ': bs) (wb ': wbs)
  where
  type
    OrderWithSymbolsNSBs (a ': as) (wa ': was) (s ': ss) =
      OrderWithSymbolsBs (a ': as) (wa ': was) (s ': ss)

  type
    OrderWithSymbolsNSWbs (a ': as) (wa ': was) (s ': ss) =
      OrderWithSymbolsWbs (a ': as) (wa ': was) (s ': ss)

  orderNS :: forall (f :: k -> Type). NS f (a ': as) -> NS f (b ': bs)
  orderNS xs =
    hcollapse $ hap yInjections xs
    where
      yInjections :: NP (Injection f (b ': bs)) (a ': as)
      yInjections = orderNP @k @(b ': bs) @(wb ': wbs) @sas injections

class
  ( bss ~ OrderWithSymbolsNPsBss ass sass sss,
    SListI2 ass,
    SListI2 sass,
    SListI2 sss
  ) =>
  OrderWithSymbolsNPs
    (ass :: [[k]])
    (sass :: [[Symbol]])
    (sss :: [[Symbol]])
    (bss :: [[k]])
  where
  type OrderWithSymbolsNPsBss ass sass sss :: [[k]]

  orderNPs :: NS (NP f) ass -> NS (NP f) bss

  orderNPsInjections :: NP (Injection (NP f) bss) ass

instance OrderWithSymbolsNPs '[] '[] '[] '[] where
  type OrderWithSymbolsNPsBss '[] '[] '[] = '[]

  orderNPs :: NS (NP f) '[] -> NS (NP f) '[]
  orderNPs xss = case xss of {}

  orderNPsInjections :: NP (Injection (NP f) '[]) '[]
  orderNPsInjections = Nil

instance
  forall
    k
    (as :: [k])
    (ass :: [[k]])
    (sas :: [Symbol])
    (sass :: [[Symbol]])
    (ss :: [Symbol])
    (sss :: [[Symbol]])
    (bs :: [k])
    (bss :: [[k]])
    (was :: [SymbolWithSymbols])
    (wbs :: [SymbolWithSymbols]).
  ( was ~ ZipSymbolsWithNoSymbols sas,
    OrderWithSymbols as was ss bs wbs,
    OrderWithSymbolsNPs ass sass sss bss,
    SListI2 (as ': ass),
    SListI2 (sas ': sass),
    SListI2 (ss ': sss)
  ) =>
  OrderWithSymbolsNPs (as ': ass) (sas ': sass) (ss ': sss) (bs ': bss)
  where
  type
    OrderWithSymbolsNPsBss (as ': ass) (sas ': sass) (ss ': sss) =
      OrderWithSymbolsBs as (ZipSymbolsWithNoSymbols sas) ss
        ': OrderWithSymbolsNPsBss ass sass sss

  orderNPs ::
    forall (f :: k -> Type).
    NS (NP f) (as ': ass) ->
    NS (NP f) (bs ': bss)
  orderNPs xss =
    hcollapse $
      hap (orderNPsInjections @k @(as ': ass) @(sas ': sass) @(ss ': sss)) xss

  orderNPsInjections ::
    forall (f :: k -> Type).
    NP (Injection (NP f) (bs : bss)) (as : ass)
  orderNPsInjections =
    fn (K . Z . orderNP @k @as @was @ss)
      :* hmap
        shiftInjection
        (orderNPsInjections @k @ass @sass @sss)

-- Wrapping

genericUnwrapTransformer ::
  (IsWrappedType a b', Transformable b' b) => Transformer a b
genericUnwrapTransformer = transformer <<^ wrappedTypeFrom

genericWrapTransformer ::
  (IsWrappedType b a', Transformable a a') => Transformer a b
genericWrapTransformer = wrappedTypeTo ^<< transformer

-- Coercion

-- | A transformer that is derived from a 'Coercible' instance. This transformer
-- does not fail.
coerceTransformer :: (Coercible a b) => Transformer a b
coerceTransformer = arr coerce
