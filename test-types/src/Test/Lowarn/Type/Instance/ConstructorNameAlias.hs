{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module                  : Test.Lowarn.Type.Instance.ConstructorNameAlias
-- SPDX-License-Identifier : MIT
-- Stability               : experimental
-- Portability             : non-portable (GHC)
--
-- Module for constructor name aliases for testing lowarn-transformer.
module Test.Lowarn.Type.Instance.ConstructorNameAlias () where

import Lowarn.Transformer (ConstructorNameAlias)

instance ConstructorNameAlias "VariantRecord3A" "VariantRecord3'A"

instance ConstructorNameAlias "VariantRecord3B" "VariantRecord3'B"

instance ConstructorNameAlias "VariantRecord3C" "VariantRecord3'C"

instance ConstructorNameAlias "VariantTuple1A" "VariantTuple1'A"
