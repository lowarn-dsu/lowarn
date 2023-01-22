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
  { _tag :: String
  }

data State = State
  { _users :: [User],
    _in :: Handle,
    _out :: Handle
  }

showUser :: User -> String
showUser = _tag

eventLoop :: RuntimeData a -> State -> IO State
eventLoop runtimeData state@(State users in_ out) = do
  continue <- isUpdateAvailable runtimeData
  if not continue
    then do
      hPutStrLn out "Following:"
      mapM_ (hPutStrLn out . showUser) $ reverse users
      hPutStrLn out "------"
      tag <- User <$> getTag
      eventLoop runtimeData $ state {_users = tag : users}
    else return state
  where
    getTag :: IO String
    getTag = do
      hPutStrLn out "Input tag of user to follow:"
      hFlush out
      tag <- hGetLine in_
      if tag =~ "\\`[a-zA-Z]+#[0-9]{4}\\'"
        then return tag
        else hPutStrLn out "Invalid tag, try again." >> getTag
