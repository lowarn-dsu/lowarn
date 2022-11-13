module Lowarn.Programs.Program1 (program, User (..)) where

import Lowarn (isUpdateAvailable)
import Lowarn.Types
  ( Program (..),
    RuntimeData (..),
    UpdateInfo (..),
  )
import System.IO (hFlush, stdout)

newtype User = User
  { _username :: String
  }

instance Show User where
  show (User username) = username

program :: Program [User] ()
program =
  Program
    ( \runtimeData ->
        eventLoop runtimeData $ maybe [] _lastState $ _updateInfo runtimeData
    )
    (const $ return Nothing)

eventLoop :: RuntimeData a -> [User] -> IO [User]
eventLoop runtimeData users = do
  continue <- isUpdateAvailable runtimeData
  if not continue
    then do
      putStrLn "Users:"
      mapM_ print users
      putStrLn "------"
      putStr "Username: "
      hFlush stdout
      user <- User <$> getLine
      eventLoop runtimeData $ user : users
    else return users
