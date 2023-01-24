{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Test.Lowarn.Types.TypesD
  ( Variant1 (..),
    Variant1' (..),
    Record1 (..),
    Record2 (..),
  )
where

import Lowarn.Transformer (deriveGeneric)

data Variant1 = Variant1'
  deriving (Show)

data Variant1' = Variant1
  deriving (Show)

data Record1 = Record1
  { _record1' :: Int
  }
  deriving (Show)

data Record2 = Record2
  { _record2B :: Int,
    _record2A :: String
  }
  deriving (Show)

deriveGeneric ''Variant1
deriveGeneric ''Variant1'
deriveGeneric ''Record1
deriveGeneric ''Record2
