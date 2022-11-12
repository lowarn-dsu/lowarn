module Lowarn.Programs.Program1
  ( program,
    User (..),
  )
where

import Lowarn.Types (Program (Program))
import System.IO (hFlush, stdout)

data User = User
  { username :: String,
    discriminator :: Int
  }
  deriving (Show)

program :: Program () [User] ()
program = Program (\() -> eventLoop []) (\_ -> ())

eventLoop :: [User] -> IO [User]
eventLoop users = do
  putStrLn "Users:"
  mapM_ print users
  putStr "Add user or exit: "
  hFlush stdout
  input <- getLine
  if input == "exit"
    then return users
    else do
      let user = User input 1234
      eventLoop $ user : users
