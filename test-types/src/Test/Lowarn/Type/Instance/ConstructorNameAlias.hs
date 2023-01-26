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

instance ConstructorNameAlias "VariantUnlabelled3A" "VariantUnlabelled3'A"

instance ConstructorNameAlias "VariantUnlabelled3B" "VariantUnlabelled3'B"

instance ConstructorNameAlias "VariantUnlabelled3C" "VariantUnlabelled3'C"

instance ConstructorNameAlias "VariantRecord3A" "VariantRecord3'A"

instance ConstructorNameAlias "VariantRecord3B" "VariantRecord3'B"

instance ConstructorNameAlias "VariantRecord3C" "VariantRecord3'C"
