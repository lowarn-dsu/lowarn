module Lowarn.ExampleProgram.Following
  ( User (..),
    State (..),
    eventLoop,
  )
where

import Lowarn.Runtime (RuntimeData, isUpdateAvailable)
import System.IO
  ( Handle,
    hFlush,
    hGetLine,
    hPrint,
    hPutStrLn,
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

eventLoop :: RuntimeData a -> State -> IO State
eventLoop runtimeData state@(State users in_ out) = do
  continue <- isUpdateAvailable runtimeData
  if not continue
    then do
      hPutStrLn out "Following:"
      mapM_ (hPrint out) $ reverse users
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
