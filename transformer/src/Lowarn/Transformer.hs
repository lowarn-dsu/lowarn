{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
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
  forall a b was cs wcs.
  ( TransformableCodes' cs (Code b),
    DatatypesMatchReordering a b was cs wcs
  ) =>
  Transformer a b
genericReorderingTransformer =
  Transformer $
    fmap (fmap to)
      . genericTransform'
      . (reorderConstructors @a @b @was @cs @wcs)
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
      wcs
  ) =>
  DatatypesMatchReordering a b was cs wcs
    | a b -> was cs wcs
  where
  reorderConstructors :: SOP f (Code a) -> SOP f cs

instance
  ( HasDatatypeInfo a,
    HasDatatypeInfo b,
    DatatypeNameAlias
      (DatatypeNameOf (DatatypeInfoOf a))
      (DatatypeNameOf (DatatypeInfoOf b)),
    SListI (ConstructorNamesOf (ConstructorInfosOf (DatatypeInfoOf a))),
    SListI (FieldNamesOfConstructors (ConstructorInfosOf (DatatypeInfoOf a))),
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
      wcs
  ) =>
  DatatypesMatchReordering a b was cs wcs
  where
  reorderConstructors ::
    forall f. SOP f (Code a) -> SOP f cs
  reorderConstructors sop =
    SOP $
      fst $
        orderNS
          (unSOP sop)
          ( zipSList
              ( sList ::
                  SList
                    ( ConstructorNamesOf (ConstructorInfosOf (DatatypeInfoOf a))
                    )
              )
              ( sList ::
                  SList
                    ( FieldNamesOfConstructors
                        (ConstructorInfosOf (DatatypeInfoOf a))
                    )
              )
          )
          ( sList ::
              SList (ConstructorNamesOf (ConstructorInfosOf (DatatypeInfoOf b)))
          )

class
  ( as ~ (Head as ': Tail as),
    was ~ (Head was ': Tail was)
  ) =>
  TakeWithSymbols
    (as :: [k])
    (was :: [(Symbol, wk)])
    (s :: Symbol)
    (b :: k)
    (wb :: (Symbol, wk))
    (cs :: [k])
    (wcs :: [(Symbol, wk)])
    | as was s -> b wb cs wcs
  where
  takeWithSymbols ::
    NP f as ->
    SList was ->
    Proxy s ->
    (f b, Proxy wb, NP f cs, SList wcs)

  takeFromSList ::
    SList as ->
    SList was ->
    Proxy s ->
    (Proxy b, Proxy wb, SList cs, SList wcs)

class
  ( as ~ (Head as ': Tail as),
    was ~ (Head was ': Tail was)
  ) =>
  TakeWithSymbols'
    (p :: Bool)
    (as :: [k])
    (was :: [(Symbol, wk)])
    (s :: Symbol)
    (b :: k)
    (wb :: (Symbol, wk))
    (cs :: [k])
    (wcs :: [(Symbol, wk)])
    | p as was s -> b wb cs wcs
  where
  takeWithSymbols' ::
    Proxy p ->
    NP f as ->
    SList was ->
    Proxy s ->
    (f b, Proxy wb, NP f cs, SList wcs)

  takeFromSList' ::
    Proxy p ->
    SList as ->
    SList was ->
    Proxy s ->
    (Proxy b, Proxy wb, SList cs, SList wcs)

class Firsts (was :: [(k1, k2)]) (sas :: [k1]) | was -> sas where
  firsts :: SList was -> SList sas

instance Firsts '[] '[] where
  firsts :: SList '[] -> SList '[]
  firsts SNil = SNil

instance Firsts was ss => Firsts ('(s, w) ': was) (s ': ss) where
  firsts :: SList ('(a, b) ': was) -> SList (a ': ss)
  firsts SCons =
    case firsts (sList :: SList was) of
      SNil -> SCons
      SCons -> SCons

class ZipSList (as :: [ak]) (bs :: [bk]) (zs :: [(ak, bk)]) | as bs -> zs where
  zipSList :: SList as -> SList bs -> SList zs

instance ZipSList '[] '[] '[] where
  zipSList :: SList '[] -> SList '[] -> SList '[]
  zipSList SNil SNil = SNil

instance ZipSList as bs zs => ZipSList (a ': as) (b ': bs) ('(a, b) ': zs) where
  zipSList :: SList (a ': as) -> SList (b ': bs) -> SList ('(a, b) ': zs)
  zipSList SCons SCons =
    case zipSList (sList :: SList as) (sList :: SList bs) of
      SNil -> SCons
      SCons -> SCons

class ZipSListWithUnit (as :: [k]) (zas :: [(k, ())]) | as -> zas where
  zipSListWithUnit :: SList as -> SList zas

instance ZipSListWithUnit '[] '[] where
  zipSListWithUnit :: SList '[] -> SList '[]
  zipSListWithUnit SNil = SNil

instance
  ZipSListWithUnit as zas =>
  ZipSListWithUnit (a ': as) ('(a, '()) ': zas)
  where
  zipSListWithUnit :: SList (a ': as) -> SList ('(a, '()) ': zas)
  zipSListWithUnit SCons =
    case zipSListWithUnit (sList :: SList as) of
      SNil -> SCons
      SCons -> SCons

instance
  ( p ~ sa `SymbolEqualsBool` s,
    TakeWithSymbols' p (a ': as) ('(sa, ra) ': was) s b wb cs wcs
  ) =>
  TakeWithSymbols (a ': as) ('(sa, ra) ': was) s b wb cs wcs
  where
  takeWithSymbols = takeWithSymbols' (Proxy :: Proxy p)
  takeFromSList = takeFromSList' (Proxy :: Proxy p)

instance TakeWithSymbols' 'True (a ': as) (wa ': was) s a wa as was where
  takeWithSymbols' ::
    Proxy 'True ->
    NP f (a ': as) ->
    SList (wa ': was) ->
    Proxy s ->
    (f a, Proxy wa, NP f as, SList was)
  takeWithSymbols' Proxy (x :* xs) SCons Proxy =
    (x, Proxy, xs, sList)

  takeFromSList' ::
    Proxy 'True ->
    SList (a ': as) ->
    SList (wa ': was) ->
    Proxy s ->
    (Proxy a, Proxy wa, SList as, SList was)
  takeFromSList' Proxy SCons SCons Proxy =
    (Proxy, Proxy, sList, sList)

instance
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
    forall f.
    Proxy 'False ->
    NP f (a1 ': a2 ': as) ->
    SList (wa1 ': wa2 ': was) ->
    Proxy s ->
    (f b, Proxy wb, NP f (a1 ': cs), SList (wa1 ': wcs))
  takeWithSymbols' Proxy (x1 :* x2 :* xs) SCons key =
    ( y,
      wy,
      x1 :* zs,
      case wzs of
        SNil -> SCons
        SCons -> SCons
    )
    where
      y :: f b
      wy :: Proxy wb
      zs :: NP f cs
      wzs :: SList wcs
      (y, wy, zs, wzs) =
        takeWithSymbols (x2 :* xs) (SCons :: SList (wa2 ': was)) key

  takeFromSList' ::
    Proxy 'False ->
    SList (a1 ': a2 ': as) ->
    SList (wa1 ': wa2 ': was) ->
    Proxy s ->
    (Proxy b, Proxy wb, SList (a1 ': cs), SList (wa1 ': wcs))
  takeFromSList' Proxy SCons SCons key =
    ( y,
      wy,
      case zs of
        SNil -> SCons
        SCons -> SCons,
      case wzs of
        SNil -> SCons
        SCons -> SCons
    )
    where
      y :: Proxy b
      wy :: Proxy wb
      zs :: SList cs
      wzs :: SList wcs
      (y, wy, zs, wzs) =
        takeFromSList
          (SCons :: SList (a2 ': as))
          (SCons :: SList (wa2 ': was))
          key

class
  OrderWithSymbols
    (as :: [k])
    (was :: [(Symbol, wk)])
    (ss :: [Symbol])
    (bs :: [k])
    (wbs :: [(Symbol, wk)])
    | as was ss -> bs wbs
  where
  orderNP ::
    NP f as ->
    SList was ->
    SList ss ->
    (NP f bs, SList wbs)

  orderSList' ::
    SList as ->
    SList was ->
    SList ss ->
    (SList bs, SList wbs)

instance OrderWithSymbols '[] '[] '[] '[] '[] where
  orderNP ::
    NP f '[] ->
    SList '[] ->
    SList '[] ->
    (NP f '[], SList '[])
  orderNP Nil SNil SNil = (Nil, SNil)

  orderSList' ::
    SList '[] ->
    SList '[] ->
    SList '[] ->
    (SList '[], SList '[])
  orderSList' SNil SNil SNil = (SNil, SNil)

instance
  ( TakeWithSymbols (a ': as) (wa ': was) s b wb ds wds,
    OrderWithSymbols ds wds ss bs wbs
  ) =>
  OrderWithSymbols (a ': as) (wa ': was) (s ': ss) (b ': bs) (wb ': wbs)
  where
  orderNP ::
    forall f.
    NP f (a ': as) ->
    SList (wa ': was) ->
    SList (s ': ss) ->
    (NP f (b ': bs), SList (wb ': wbs))
  orderNP (x :* xs) SCons SCons =
    ( y :* ys,
      case wys of
        SNil -> SCons
        SCons -> SCons
    )
    where
      y :: f b
      ws :: NP f ds
      wws :: SList wds
      (y, _, ws, wws) =
        takeWithSymbols
          (x :* xs)
          (SCons :: SList (wa ': was))
          (Proxy :: Proxy s)

      ys :: NP f bs
      wys :: SList wbs
      (ys, wys) =
        orderNP ws wws (sList :: SList ss)

  orderSList' ::
    SList (a ': as) ->
    SList (wa ': was) ->
    SList (s ': ss) ->
    (SList (b ': bs), SList (wb ': wbs))
  orderSList' SCons SCons SCons =
    ( case ys of
        SNil -> SCons
        SCons -> SCons,
      case wys of
        SNil -> SCons
        SCons -> SCons
    )
    where
      ws :: SList ds
      wws :: SList wds
      (_, _, ws, wws) =
        takeFromSList
          (SCons :: SList (a ': as))
          (SCons :: SList (wa ': was))
          (Proxy :: Proxy s)

      ys :: SList bs
      wys :: SList wbs
      (ys, wys) =
        orderSList' ws wws (sList :: SList ss)

class
  ( OrderWithSymbols as was ss bs wbs
  ) =>
  OrderWithSymbolsNS
    (as :: [k])
    (was :: [(Symbol, wk)])
    (ss :: [Symbol])
    (bs :: [k])
    (wbs :: [(Symbol, wk)])
    | as was ss bs -> wbs
  where
  orderNS ::
    NS f as ->
    SList was ->
    SList ss ->
    (NS f bs, SList wbs)

instance
  ( OrderWithSymbols (a ': as) (wa ': was) (s ': ss) (b ': bs) (wb ': wbs),
    Firsts (wa ': was) sas,
    OrderWithSymbols
      (b ': bs)
      (wb ': wbs)
      sas
      (a ': as)
      (wa ': was),
    SListI (b ': bs),
    SListI (a ': as)
  ) =>
  OrderWithSymbolsNS (a ': as) (wa ': was) (s ': ss) (b ': bs) (wb ': wbs)
  where
  orderNS ::
    forall f.
    NS f (a ': as) ->
    SList (wa ': was) ->
    SList (s ': ss) ->
    (NS f (b ': bs), SList (wb ': wbs))
  orderNS xs SCons SCons =
    (hcollapse $ hap yInjections xs, wys)
    where
      wys :: SList (wb ': wbs)
      (_, wys) =
        orderSList'
          (SCons :: SList (a ': as))
          (SCons :: SList (wa ': was))
          (SCons :: SList (s ': ss))

      yInjections ::
        NP (Injection f (b ': bs)) (a ': as)
      (yInjections, _) =
        orderNP injections wys (firsts (SCons :: SList (wa ': was)))

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
