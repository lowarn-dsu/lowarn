{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Test.Lowarn.Type.TypesC
  ( Variant1 (..),
    Record1 (..),
    Variant2 (..),
    Record2 (..),
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

data Record2 = Record2A
  { _record2B :: String,
    _record2A :: Int
  }
  deriving (Show)

deriveGeneric ''Variant1
deriveGeneric ''Record1
deriveGeneric ''Variant2
deriveGeneric ''Record2
