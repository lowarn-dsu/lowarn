{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

-- |
-- Module                  : Test.Lowarn.Type.TypesC
-- SPDX-License-Identifier : MIT
-- Stability               : experimental
-- Portability             : non-portable (GHC)
--
-- Module for types for testing lowarn-transformer.
module Test.Lowarn.Type.TypesC
  ( Variant1 (..),
    Record1 (..),
    Variant2 (..),
    Unlabelled2 (..),
    Record2 (..),
    VariantUnlabelled3 (..),
    VariantRecord3 (..),
  )
where

import Lowarn.Transformer (deriveGeneric)

data Variant1 = Variant1'A
  deriving (Show)

data Record1 = Record1A
  { _record1' :: Int
  }
  deriving (Show)

data Variant2 = Variant2B | Variant2A
  deriving (Show)

data Unlabelled2 = Unlabelled2A String Int
  deriving (Show)

data Record2 = Record2A
  { _record2B :: String,
    _record2A :: Int
  }
  deriving (Show)

data VariantRecord3
  = VariantRecord3A Int String Bool
  | VariantRecord3B Bool Int String
  | VariantRecord3C String Bool Int
  deriving (Show)

data VariantUnlabelled3
  = VariantUnlabelled3C Int Bool String
  | VariantUnlabelled3A Bool String Int
  | VariantUnlabelled3B String Int Bool
  deriving (Show)

deriveGeneric ''Variant1
deriveGeneric ''Record1
deriveGeneric ''Variant2
deriveGeneric ''Unlabelled2
deriveGeneric ''Record2
deriveGeneric ''VariantUnlabelled3
deriveGeneric ''VariantRecord3
