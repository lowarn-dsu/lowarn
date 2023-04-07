{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

-- |
-- Module                  : Test.Lowarn.Type.TypesD
-- SPDX-License-Identifier : MIT
-- Stability               : experimental
-- Portability             : non-portable (POSIX, GHC)
--
-- Module for types for testing lowarn-transformer.
module Test.Lowarn.Type.TypesD
  ( Variant1 (..),
    Record1 (..),
    Record2 (..),
    VariantUnlabelled3 (..),
  )
where

import Lowarn.Transformer (deriveGeneric)

data Variant1 = Variant1A | Variant1B
  deriving (Show)

data Record1 = Record1A
  { record1A :: Int,
    record1B :: String
  }
  deriving (Show)

data Record2 = Record2A
  { record2B :: Int,
    record2A :: String
  }
  deriving (Show)

data VariantUnlabelled3
  = VariantUnlabelled3A
      { variantUnlabelled3A :: Int,
        variantUnlabelled3B :: String,
        variantUnlabelled3C :: Bool
      }
  | VariantUnlabelled3B
      { variantUnlabelled3C :: Bool,
        variantUnlabelled3A :: Int,
        variantUnlabelled3B :: String
      }
  | VariantUnlabelled3C
      { variantUnlabelled3B :: String,
        variantUnlabelled3C :: Bool,
        variantUnlabelled3A :: Int
      }
  deriving (Show)

deriveGeneric ''Variant1
deriveGeneric ''Record1
deriveGeneric ''Record2
deriveGeneric ''VariantUnlabelled3
