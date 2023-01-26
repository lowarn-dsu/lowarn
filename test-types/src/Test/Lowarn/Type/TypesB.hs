{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

-- |
-- Module                  : Test.Lowarn.Type.TypesB
-- SPDX-License-Identifier : MIT
-- Stability               : experimental
-- Portability             : non-portable (GHC)
--
-- Module for types for testing lowarn-transformer.
module Test.Lowarn.Type.TypesB
  ( Variant1 (..),
    Unlabelled1 (..),
    Record1 (..),
    Record1' (..),
    Variant2 (..),
    Unlabelled2 (..),
    Record2 (..),
    Variant3 (..),
    Record3 (..),
    VariantUnlabelled3 (..),
    VariantRecord3 (..),
    Variant1Wrapper (..),
  )
where

import Lowarn.Transformer (deriveGeneric)

data Variant1 = Variant1A
  deriving (Show)

data Unlabelled1 = Unlabelled1A Int
  deriving (Show)

data Record1 = Record1A
  { _record1A :: Int
  }
  deriving (Show)

data Record1' = Record1'A
  { _record1A' :: Int
  }
  deriving (Show)

data Variant2 = Variant2A | Variant2B
  deriving (Show)

data Unlabelled2 = Unlabelled2A Int String
  deriving (Show)

data Record2 = Record2A
  { _record2A :: Int,
    _record2B :: String
  }
  deriving (Show)

data Variant3 = Variant3C | Variant3A | Variant3B
  deriving (Show)

data Record3 = Record3A
  { _record3C :: Bool,
    _record3A :: Int,
    _record3B :: String
  }
  deriving (Show)

data VariantUnlabelled3
  = VariantUnlabelled3C Int Bool String
  | VariantUnlabelled3A Bool String Int
  | VariantUnlabelled3B String Int Bool
  deriving (Show)

data VariantRecord3
  = VariantRecord3C
      { _variantRecord3A :: Int,
        _variantRecord3C :: Bool,
        _variantRecord3B :: String
      }
  | VariantRecord3A
      { _variantRecord3C :: Bool,
        _variantRecord3B :: String,
        _variantRecord3A :: Int
      }
  | VariantRecord3B
      { _variantRecord3B :: String,
        _variantRecord3A :: Int,
        _variantRecord3C :: Bool
      }
  deriving (Show)

data Variant1Wrapper = Variant1Wrapper Variant1
  deriving (Show)

deriveGeneric ''Variant1
deriveGeneric ''Unlabelled1
deriveGeneric ''Record1
deriveGeneric ''Record1'
deriveGeneric ''Variant2
deriveGeneric ''Unlabelled2
deriveGeneric ''Record2
deriveGeneric ''Variant3
deriveGeneric ''Record3
deriveGeneric ''VariantUnlabelled3
deriveGeneric ''VariantRecord3
deriveGeneric ''Variant1Wrapper
