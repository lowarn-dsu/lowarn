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
    SListI (ConstructorNamesOf (ConstructorInfosOf (DatatypeInfoOf a))),
    SListI (ConstructorNamesOf (ConstructorInfosOf (DatatypeInfoOf b))),
    ZipSList
      (ConstructorNamesOf (ConstructorInfosOf (DatatypeInfoOf a)))
      (FieldNamesOfConstructors (ConstructorInfosOf (DatatypeInfoOf a)))
      was,
    OrderWithSymbolsNS
      (Code a)
      was
      (ConstructorNamesOf (ConstructorInfosOf (DatatypeInfoOf b)))
      cs
      wcs,
    Seconds wcs scss,
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
    (was :: [(Symbol, [Symbol])])
    (cs :: [[Type]])
    (wcs :: [(Symbol, [Symbol])])
    (scss :: [[Symbol]])
    (ds :: [[Type]]).
  ( HasDatatypeInfo a,
    HasDatatypeInfo b,
    DatatypeNameAlias
      (DatatypeNameOf (DatatypeInfoOf a))
      (DatatypeNameOf (DatatypeInfoOf b)),
    SListI (ConstructorNamesOf (ConstructorInfosOf (DatatypeInfoOf a))),
    SListI (ConstructorNamesOf (ConstructorInfosOf (DatatypeInfoOf b))),
    SListI (FieldNamesOfConstructors (ConstructorInfosOf (DatatypeInfoOf a))),
    SListI (FieldNamesOfConstructors (ConstructorInfosOf (DatatypeInfoOf b))),
    ZipSList
      (ConstructorNamesOf (ConstructorInfosOf (DatatypeInfoOf a)))
      (FieldNamesOfConstructors (ConstructorInfosOf (DatatypeInfoOf a)))
      was,
    OrderWithSymbolsNS
      (Code a)
      was
      (ConstructorNamesOf (ConstructorInfosOf (DatatypeInfoOf b)))
      cs
      wcs,
    Seconds wcs scss,
    OrderWithSymbolsNPs
      cs
      scss
      (FieldNamesOfConstructors (ConstructorInfosOf (DatatypeInfoOf b)))
      ds
  ) =>
  DatatypesMatchReordering a b was cs wcs scss ds
  where
  reorderConstructors ::
    forall f. SOP f (Code a) -> SOP f ds
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

class Firsts (ws :: [(lk, rk)]) (ls :: [lk]) | ws -> ls

instance Firsts '[] '[]

instance Firsts ws ls => Firsts ('(l, r) ': ws) (l ': ls)

class (SListI ws) => Seconds (ws :: [(lk, rk)]) (rs :: [rk]) | ws -> rs

instance Seconds '[] '[]

instance Seconds ws rs => Seconds ('(l, r) ': ws) (r ': rs)

class ZipSList (as :: [ak]) (bs :: [bk]) (zs :: [(ak, bk)]) | as bs -> zs

instance ZipSList '[] '[] '[]

instance ZipSList as bs zs => ZipSList (a ': as) (b ': bs) ('(a, b) ': zs)

class ZipSListWithEmptyList (as :: [k]) (zas :: [(k, [Symbol])]) | as -> zas

instance ZipSListWithEmptyList '[] '[]

instance
  ZipSListWithEmptyList as zas =>
  ZipSListWithEmptyList (a ': as) ('(a, '[]) ': zas)

class
  ( as ~ (Head as ': Tail as),
    was ~ (Head was ': Tail was)
  ) =>
  TakeWithSymbols
    (as :: [k])
    (was :: [(Symbol, [Symbol])])
    (s :: Symbol)
    (b :: k)
    (wb :: (Symbol, [Symbol]))
    (cs :: [k])
    (wcs :: [(Symbol, [Symbol])])
    | as was s -> b wb cs wcs
  where
  takeWithSymbols :: NP f as -> (f b, NP f cs)

class
  ( as ~ (Head as ': Tail as),
    was ~ (Head was ': Tail was)
  ) =>
  TakeWithSymbols'
    (p :: Bool)
    (as :: [k])
    (was :: [(Symbol, [Symbol])])
    (s :: Symbol)
    (b :: k)
    (wb :: (Symbol, [Symbol]))
    (cs :: [k])
    (wcs :: [(Symbol, [Symbol])])
    | p as was s -> b wb cs wcs
  where
  takeWithSymbols' :: NP f as -> (f b, NP f cs)

instance
  forall
    k
    (p :: Bool)
    (a :: k)
    (as :: [k])
    (sa :: Symbol)
    (ra :: [Symbol])
    (was :: [(Symbol, [Symbol])])
    (s :: Symbol)
    (b :: k)
    (wb :: (Symbol, [Symbol]))
    (cs :: [k])
    (wcs :: [(Symbol, [Symbol])]).
  ( p ~ sa `SymbolEqualsBool` s,
    TakeWithSymbols' p (a ': as) ('(sa, ra) ': was) s b wb cs wcs
  ) =>
  TakeWithSymbols (a ': as) ('(sa, ra) ': was) s b wb cs wcs
  where
  takeWithSymbols = takeWithSymbols' @k @p @(a ': as) @('(sa, ra) ': was) @s

instance TakeWithSymbols' 'True (a ': as) (wa ': was) s a wa as was where
  takeWithSymbols' :: NP f (a ': as) -> (f a, NP f as)
  takeWithSymbols' (x :* xs) = (x, xs)

instance
  forall
    k
    (a1 :: k)
    (a2 :: k)
    (as :: [k])
    (wa1 :: (Symbol, [Symbol]))
    (wa2 :: (Symbol, [Symbol]))
    (was :: [(Symbol, [Symbol])])
    (s :: Symbol)
    (b :: k)
    (wb :: (Symbol, [Symbol]))
    (cs :: [k])
    (wcs :: [(Symbol, [Symbol])]).
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
  takeWithSymbols' ::
    forall (f :: k -> Type).
    NP f (a1 ': a2 ': as) ->
    (f b, NP f (a1 ': cs))
  takeWithSymbols' (x1 :* x2 :* xs) =
    ( y,
      x1 :* zs
    )
    where
      y :: f b
      zs :: NP f cs
      (y, zs) = takeWithSymbols @k @(a2 ': as) @(wa2 ': was) @s (x2 :* xs)

class
  (SListI was) =>
  OrderWithSymbols
    (as :: [k])
    (was :: [(Symbol, [Symbol])])
    (ss :: [Symbol])
    (bs :: [k])
    (wbs :: [(Symbol, [Symbol])])
    | as was ss -> bs wbs
  where
  orderNP :: NP f as -> NP f bs

instance OrderWithSymbols '[] '[] '[] '[] '[] where
  orderNP :: NP f '[] -> NP f '[]
  orderNP Nil = Nil

instance
  forall
    k
    (a :: k)
    (as :: [k])
    (wa :: (Symbol, [Symbol]))
    (was :: [(Symbol, [Symbol])])
    (s :: Symbol)
    (ss :: [Symbol])
    (b :: k)
    (bs :: [k])
    (wb :: (Symbol, [Symbol]))
    (wbs :: [(Symbol, [Symbol])])
    (ds :: [k])
    (wds :: [(Symbol, [Symbol])]).
  ( TakeWithSymbols (a ': as) (wa ': was) s b wb ds wds,
    OrderWithSymbols ds wds ss bs wbs,
    SListI was
  ) =>
  OrderWithSymbols (a ': as) (wa ': was) (s ': ss) (b ': bs) (wb ': wbs)
  where
  orderNP ::
    forall f.
    NP f (a ': as) ->
    NP f (b ': bs)
  orderNP (x :* xs) =
    y :* ys
    where
      y :: f b
      ws :: NP f ds
      (y, ws) =
        takeWithSymbols
          @k
          @(a ': as)
          @(wa ': was)
          @s
          (x :* xs)

      ys :: NP f bs
      ys = orderNP @k @ds @wds @ss ws

class
  ( OrderWithSymbols as was ss bs wbs,
    SListI as,
    SListI was,
    SListI ss,
    SListI bs
  ) =>
  OrderWithSymbolsNS
    (as :: [k])
    (was :: [(Symbol, [Symbol])])
    (ss :: [Symbol])
    (bs :: [k])
    (wbs :: [(Symbol, [Symbol])])
    | as was ss -> bs wbs
  where
  orderNS ::
    NS f as ->
    NS f bs

instance
  forall
    k
    (a :: k)
    (as :: [k])
    (wa :: (Symbol, [Symbol]))
    (was :: [(Symbol, [Symbol])])
    (s :: Symbol)
    (ss :: [Symbol])
    (b :: k)
    (bs :: [k])
    (wb :: (Symbol, [Symbol]))
    (wbs :: [(Symbol, [Symbol])])
    (sas :: [Symbol]).
  ( OrderWithSymbols (a ': as) (wa ': was) (s ': ss) (b ': bs) (wb ': wbs),
    Firsts (wa ': was) sas,
    OrderWithSymbols
      (b ': bs)
      (wb ': wbs)
      sas
      (a ': as)
      (wa ': was),
    SListI (a ': as),
    SListI (wa ': was),
    SListI (s ': ss),
    SListI (b ': bs)
  ) =>
  OrderWithSymbolsNS (a ': as) (wa ': was) (s ': ss) (b ': bs) (wb ': wbs)
  where
  orderNS ::
    forall f.
    NS f (a ': as) ->
    NS f (b ': bs)
  orderNS xs =
    hcollapse $ hap yInjections xs
    where
      yInjections :: NP (Injection f (b ': bs)) (a ': as)
      yInjections = orderNP @k @(b ': bs) @(wb ': wbs) @sas injections

class
  (SListI2 ass, SListI2 sass, SListI2 sss) =>
  OrderWithSymbolsNPs
    (ass :: [[k]])
    (sass :: [[Symbol]])
    (sss :: [[Symbol]])
    (bss :: [[k]])
    | ass sass sss -> bss
  where
  orderNPs ::
    NS (NP f) ass ->
    NS (NP f) bss

  orderNPsInjections ::
    NP (Injection (NP f) bss) ass

instance OrderWithSymbolsNPs '[] '[] '[] '[] where
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
    (was :: [(Symbol, [Symbol])])
    (wbs :: [(Symbol, [Symbol])]).
  ( ZipSListWithEmptyList sas was,
    OrderWithSymbols as was ss bs wbs,
    OrderWithSymbolsNPs ass sass sss bss,
    SListI2 (as ': ass),
    SListI2 (sas ': sass),
    SListI2 (ss ': sss)
  ) =>
  OrderWithSymbolsNPs (as ': ass) (sas ': sass) (ss ': sss) (bs ': bss)
  where
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
