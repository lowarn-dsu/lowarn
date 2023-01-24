{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Test.Lowarn.Types.TypesE (Variant1' (..)) where

import Lowarn.Transformer (deriveGeneric)

data Variant1' = Variant1A
  deriving (Show)

deriveGeneric ''Variant1'
