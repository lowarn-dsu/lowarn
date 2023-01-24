{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Lowarn.Types.TypesC
  ( Variant1 (..),
    Variant1' (..),
    Record1 (..),
    Variant2 (..),
    Record2 (..),
  )
where

import Lowarn.Transformer (deriveGeneric)

data Variant1 = Variant1'
  deriving (Show)

data Variant1' = Variant1 | Variant2
  deriving (Show)

data Record1 = Record1
  { _record1A :: Int,
    _record1B :: String
  }
  deriving (Show)

data Variant2 = Variant2B | Variant2A
  deriving (Show)

data Record2 = Record2
  { _record2B :: String,
    _record2A :: Int
  }
  deriving (Show)

deriveGeneric ''Variant1
deriveGeneric ''Variant1'
deriveGeneric ''Record1
deriveGeneric ''Variant2
deriveGeneric ''Record2
