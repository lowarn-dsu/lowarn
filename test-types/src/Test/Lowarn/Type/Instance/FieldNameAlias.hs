{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Lowarn.Type.Instance.FieldNameAlias () where

import Lowarn.Transformer (FieldNameAlias)

instance FieldNameAlias "_variantRecord3A" "_variantRecord3'A"

instance FieldNameAlias "_variantRecord3B" "_variantRecord3'B"

instance FieldNameAlias "_variantRecord3C" "_variantRecord3'C"
