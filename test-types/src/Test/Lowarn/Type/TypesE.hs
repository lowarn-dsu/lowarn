{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

-- |
-- Module                  : Test.Lowarn.Type.TypesE
-- SPDX-License-Identifier : MIT
-- Stability               : experimental
-- Portability             : non-portable (POSIX, GHC)
--
-- Module for types for testing lowarn-transformer.
module Test.Lowarn.Type.TypesE (Variant1' (..)) where

import Lowarn.Transformer (deriveGeneric)

data Variant1' = Variant1A
  deriving (Show)

deriveGeneric ''Variant1'
