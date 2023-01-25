{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Lowarn.Type.Instance.Fail () where

import Lowarn.Transformer (Transformable (transform))
import qualified Test.Lowarn.Type.TypesA as A
import qualified Test.Lowarn.Type.TypesB as B

instance Transformable A.Variant2 B.Variant2 where
  transform :: A.Variant2 -> IO (Maybe B.Variant2)
  transform A.Variant2B = return $ Just B.Variant2B
  transform A.Variant2A = return Nothing
