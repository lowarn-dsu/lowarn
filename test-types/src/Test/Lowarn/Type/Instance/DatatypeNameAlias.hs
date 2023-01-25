{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Lowarn.Type.Instance.DatatypeNameAlias () where

import Lowarn.Transformer (DatatypeNameAlias)

instance DatatypeNameAlias "VariantRecord3" "VariantRecord3'"

instance DatatypeNameAlias "VariantTuple1" "VariantTuple1'"
