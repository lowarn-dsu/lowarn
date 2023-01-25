{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module                  : Test.Lowarn.Type.Instance.DatatypeNameAlias
-- SPDX-License-Identifier : MIT
-- Stability               : experimental
-- Portability             : non-portable (GHC)
--
-- Module for datatype name aliases for testing lowarn-transformer.
module Test.Lowarn.Type.Instance.DatatypeNameAlias () where

import Lowarn.Transformer (DatatypeNameAlias)

instance DatatypeNameAlias "VariantRecord3" "VariantRecord3'"

instance DatatypeNameAlias "VariantTuple1" "VariantTuple1'"