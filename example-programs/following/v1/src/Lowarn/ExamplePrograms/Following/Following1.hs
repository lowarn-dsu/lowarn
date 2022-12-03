module Lowarn.ExamplePrograms.Following.Following1
  ( program,
    User (..),
    State (..),
  )
where

import Data.Maybe (fromMaybe)
import Lowarn.Runtime (Program (..), RuntimeData, isUpdateAvailable, lastState)
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
  { _username :: String
  }

instance Show User where
  show (User username) = username

data State = State
  { _users :: [User],
    _in :: Handle,
    _out :: Handle
  }

program :: Program State (Handle, Handle)
program =
  Program
    ( \runtimeData ->
        eventLoop runtimeData $
          fromMaybe (State [] stdin stdout) (lastState runtimeData)
    )
    (\(in_, out) -> return $ Just $ State [] in_ out)

eventLoop :: RuntimeData a -> State -> IO State
eventLoop runtimeData state@(State users in_ out) = do
  continue <- isUpdateAvailable runtimeData
  if not continue
    then do
      hPutStrLn out "Following:"
      mapM_ (hPrint out) users
      hPutStrLn out "------"
      user <- User <$> getUsername
      eventLoop runtimeData $ state {_users = users ++ [user]}
    else return state
  where
    getUsername :: IO String
    getUsername = do
      hPutStrLn out "Input username of user to follow:"
      hFlush out
      username <- hGetLine in_
      if username =~ "\\`[a-zA-Z]+\\'"
        then return username
        else hPutStrLn out "Invalid username, try again." >> getUsername
