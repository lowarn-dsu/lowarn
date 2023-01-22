{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Spec.Transformers.TypesD
  ( Variant1 (..),
    Variant1' (..),
    Record1 (..),
    Record2 (..),
  )
where

import Lowarn.Transformer (deriveGeneric)

data Variant1 = Variant1'

data Variant1' = Variant1

data Record1 = Record1
  { _record1' :: Int
  }

data Record2 = Record2
  { _record2B :: Int,
    _record2A :: String
  }

deriveGeneric ''Variant1
deriveGeneric ''Variant1'
deriveGeneric ''Record1
deriveGeneric ''Record2
