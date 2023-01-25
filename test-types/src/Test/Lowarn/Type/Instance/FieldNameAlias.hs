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

import Lowarn.Transformer (FieldNameAlias)

instance FieldNameAlias "_variantRecord3A" "_variantRecord3'A"

instance FieldNameAlias "_variantRecord3B" "_variantRecord3'B"

instance FieldNameAlias "_variantRecord3C" "_variantRecord3'C"
