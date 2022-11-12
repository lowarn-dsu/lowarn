{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lowarn.Types (Program (..)) where

data Program a b c = Program
  { _program :: a -> IO b,
    _transformer :: c -> IO a
  }
