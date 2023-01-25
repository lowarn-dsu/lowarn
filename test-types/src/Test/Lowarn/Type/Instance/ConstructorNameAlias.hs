{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Lowarn.Type.Instance.ConstructorNameAlias () where

import Lowarn.Transformer (ConstructorNameAlias)

instance ConstructorNameAlias "VariantRecord3A" "VariantRecord3'A"

instance ConstructorNameAlias "VariantRecord3B" "VariantRecord3'B"

instance ConstructorNameAlias "VariantRecord3C" "VariantRecord3'C"

instance ConstructorNameAlias "VariantTuple1A" "VariantTuple1'A"
