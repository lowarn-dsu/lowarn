module Lowarn.Programs.Program1 (program, User (..)) where

import Lowarn (isUpdateAvailable)
import Lowarn.Types
  ( Program (..),
    RuntimeData (..),
    UpdateInfo (..),
  )
import System.IO (hFlush, stdout)
import Text.Regex.TDFA

newtype User = User
  { _username :: String
  }

instance Show User where
  show (User username) = username

program :: Program [User] ()
program =
  Program
    ( \runtimeData ->
        eventLoop runtimeData $ maybe [] _lastState (_updateInfo runtimeData)
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
      user <- User <$> getUsername
      eventLoop runtimeData $ user : users
    else return users
  where
    getUsername :: IO String
    getUsername = do
      putStr "Username: "
      hFlush stdout
      username <- getLine
      if username =~ "\\`[a-zA-Z]+\\'"
        then return username
        else putStrLn "Invalid username, try again." >> getUsername
