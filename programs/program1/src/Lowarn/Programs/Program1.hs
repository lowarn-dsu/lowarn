module Lowarn.Programs.Program1
  ( program,
    User (..),
  )
where

import Lowarn.Types (Program (..))
import System.IO (hFlush, stdout)

newtype User = User
  { _username :: String
  }

instance Show User where
  show (User username) = username

program :: Program () [User] ()
program = Program (\() -> eventLoop []) (\() -> return ())

eventLoop :: [User] -> IO [User]
eventLoop users = do
  putStrLn "Users:"
  mapM_ print users
  putStrLn "------"
  putStr "Username or exit: "
  hFlush stdout
  input <- getLine
  if input == "exit"
    then putStrLn "------" >> return users
    else do
      let user = User input
      eventLoop $ user : users
