module Lowarn.ExampleProgram.ManualFollowing
  ( User (..),
    State (..),
    eventLoop,
    showUser,
  )
where

import Lowarn (RuntimeData, isUpdateAvailable)
import System.IO
  ( Handle,
    hFlush,
    hGetLine,
    hPutStrLn,
  )
import Text.Regex.TDFA

newtype User = User
  { _username :: String
  }

data State = State
  { _users :: [User],
    _in :: Handle,
    _out :: Handle
  }

showUser :: User -> String
showUser = _username

eventLoop :: RuntimeData a -> State -> IO State
eventLoop runtimeData state@(State users in_ out) = do
  continue <- isUpdateAvailable runtimeData
  if not continue
    then do
      hPutStrLn out "Following:"
      mapM_ (hPutStrLn out . showUser) users
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
