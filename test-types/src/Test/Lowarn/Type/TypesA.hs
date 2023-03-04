{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

-- |
-- Module                  : Test.Lowarn.Type.TypesA
-- SPDX-License-Identifier : MIT
-- Stability               : experimental
-- Portability             : non-portable (GHC)
--
-- Module for types for testing lowarn-transformer.
module Test.Lowarn.Type.TypesA
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
    VariantUnlabelled3' (..),
    VariantRecord3 (..),
    VariantRecord3' (..),
    Variant1Wrapper (..),
    Variant1Wrapper' (..),
    Variant1Wrapper'' (..),
  )
where

import Lowarn.Transformer (deriveGeneric)

data Variant1 = Variant1A
  deriving (Show)

data Unlabelled1 = Unlabelled1A Int
  deriving (Show)

data Record1 = Record1A
  { record1A :: Int
  }
  deriving (Show)

newtype Record1' = Record1'A
  { record1A' :: Int
  }
  deriving (Show)

data Variant2 = Variant2A | Variant2B
  deriving (Show)

data Unlabelled2 = Unlabelled2A Int String
  deriving (Show)

data Record2 = Record2A
  { record2A :: Int,
    record2B :: String
  }
  deriving (Show)

data Variant3 = Variant3A | Variant3B | Variant3C
  deriving (Show)

data Record3 = Record3A
  { record3A :: Int,
    record3B :: String,
    record3C :: Bool
  }
  deriving (Show)

data VariantUnlabelled3
  = VariantUnlabelled3A Int String Bool
  | VariantUnlabelled3B Bool Int String
  | VariantUnlabelled3C String Bool Int
  deriving (Show)

data VariantUnlabelled3'
  = VariantUnlabelled3'A Int String Bool
  | VariantUnlabelled3'B Bool Int String
  | VariantUnlabelled3'C String Bool Int
  deriving (Show)

data VariantRecord3
  = VariantRecord3A
      { variantRecord3A :: Int,
        variantRecord3B :: String,
        variantRecord3C :: Bool
      }
  | VariantRecord3B
      { variantRecord3C :: Bool,
        variantRecord3A :: Int,
        variantRecord3B :: String
      }
  | VariantRecord3C
      { variantRecord3B :: String,
        variantRecord3C :: Bool,
        variantRecord3A :: Int
      }
  deriving (Show)

data VariantRecord3'
  = VariantRecord3'A
      { variantRecord3'A :: Int,
        variantRecord3'B :: String,
        variantRecord3'C :: Bool
      }
  | VariantRecord3'B
      { variantRecord3'C :: Bool,
        variantRecord3'A :: Int,
        variantRecord3'B :: String
      }
  | VariantRecord3'C
      { variantRecord3'B :: String,
        variantRecord3'C :: Bool,
        variantRecord3'A :: Int
      }
  deriving (Show)

data Variant1Wrapper = Variant1Wrapper Variant1
  deriving (Show)

newtype Variant1Wrapper' = Variant1Wrapper' Variant1
  deriving (Show)

data Variant1Wrapper'' = Variant1Wrapper'' Variant1 Variant1
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
deriveGeneric ''VariantUnlabelled3'
deriveGeneric ''VariantRecord3
deriveGeneric ''VariantRecord3'
deriveGeneric ''Variant1Wrapper
deriveGeneric ''Variant1Wrapper'
deriveGeneric ''Variant1Wrapper''
