module Lowarn.Programs.Program3 (program, User (..)) where

import Data.Foldable (toList)
import Data.Sequence (Seq)
import Lowarn (isUpdateAvailable)
import qualified Lowarn.Programs.Program2 as Program2
import Lowarn.Types
  ( Program (..),
    RuntimeData (..),
    UpdateInfo (..),
  )
import System.IO (hFlush, stdout)
import Text.Regex.TDFA

newtype User = User
  { _tag :: String
  }

instance Show User where
  show (User tag) = tag

transformer :: Seq Program2.User -> IO (Maybe [User])
transformer =
  return
    . Just
    . toList
    . fmap (User . show)

program :: Program [User] (Seq Program2.User)
program =
  Program
    ( \runtimeData ->
        eventLoop runtimeData $ maybe [] _lastState (_updateInfo runtimeData)
    )
    transformer

eventLoop :: RuntimeData a -> [User] -> IO [User]
eventLoop runtimeData users = do
  continue <- isUpdateAvailable runtimeData
  if not continue
    then do
      putStrLn "Users:"
      mapM_ print users
      putStrLn "------"
      tag <- User <$> getTag
      eventLoop runtimeData $ tag : users
    else return users
  where
    getTag :: IO String
    getTag = do
      putStr "Tag: "
      hFlush stdout
      tag <- getLine
      if tag =~ "\\`[a-zA-Z]+#[0-9]{4}\\'"
        then return tag
        else putStrLn "Invalid tag, try again." >> getTag
