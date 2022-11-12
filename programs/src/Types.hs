{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Types (User (..), Program (..), defaultProgram) where

import Data.Typeable

data User = User
  { username :: String,
    discriminator :: Int
  }
  deriving (Show, Typeable)

data Program a b = Program
  { program :: a -> IO b
  }
  deriving (Typeable)

defaultProgram :: Program () ()
defaultProgram = Program {program = \() -> return ()}
