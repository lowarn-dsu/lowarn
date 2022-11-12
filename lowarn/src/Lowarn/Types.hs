{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lowarn.Types (Program (..)) where

data Program a b c = Program
  { program :: a -> IO b,
    transformer :: c -> a
  }
