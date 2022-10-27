module Types (User (..)) where

data User = User
  { username :: String,
    discriminator :: Int
  }
  deriving (Show)
