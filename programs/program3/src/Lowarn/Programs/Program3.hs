module Lowarn.Programs.Program3 (program, User (..)) where

import Data.Foldable (toList)
import Lowarn (isUpdateAvailable)
import qualified Lowarn.Programs.Program2 as Program2
import Lowarn.Types
  ( Program (..),
    RuntimeData (..),
    UpdateInfo (..),
  )
import System.IO
  ( Handle,
    hFlush,
    hGetLine,
    hPrint,
    hPutStrLn,
    stdin,
    stdout,
  )
import Text.Regex.TDFA

newtype User = User
  { _tag :: String
  }

instance Show User where
  show (User tag) = tag

data State = State
  { _users :: [User],
    _in :: Handle,
    _out :: Handle
  }

transformer :: Program2.State -> IO (Maybe State)
transformer (Program2.State users in_ out) =
  return $ Just $ State users' in_ out
  where
    users' = toList $ fmap (User . show) users

program :: Program State Program2.State
program =
  Program
    ( \runtimeData ->
        eventLoop runtimeData $
          maybe (State [] stdin stdout) _lastState (_updateInfo runtimeData)
    )
    transformer

eventLoop :: RuntimeData a -> State -> IO State
eventLoop runtimeData state@(State users in_ out) = do
  continue <- isUpdateAvailable runtimeData
  if not continue
    then do
      hPutStrLn out "Users:"
      mapM_ (hPrint out) users
      hPutStrLn out "------"
      tag <- User <$> getTag
      eventLoop runtimeData $ state {_users = tag : users}
    else return state
  where
    getTag :: IO String
    getTag = do
      hPutStrLn out "Tag:"
      hFlush out
      tag <- hGetLine in_
      if tag =~ "\\`[a-zA-Z]+#[0-9]{4}\\'"
        then return tag
        else hPutStrLn out "Invalid tag, try again." >> getTag
