{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module                  : Test.Lowarn.Type.Instance.FieldNameAlias
-- SPDX-License-Identifier : MIT
-- Stability               : experimental
-- Portability             : non-portable (GHC)
--
-- Module for field name aliases for testing lowarn-transformer.
module Test.Lowarn.Type.Instance.FieldNameAlias () where

import Lowarn.Transformer

instance FieldNameAlias "variantRecord3A" "variantRecord3'A"

instance FieldNameAlias "variantRecord3B" "variantRecord3'B"

instance FieldNameAlias "variantRecord3C" "variantRecord3'C"
