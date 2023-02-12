{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Lowarn.ExampleProgram.Following
  ( User (..),
    State (..),
    eventLoop,
    showUser,
  )
where

import Control.Concurrent (readMVar)
import GlobalRuntimeData_following (globalRuntimeData)
import Lowarn (isUpdateAvailable)
import Lowarn.Transformer (deriveGeneric)
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

deriveGeneric ''User
deriveGeneric ''State

showUser :: User -> String
showUser = _username

eventLoop :: State -> IO State
eventLoop state@(State users in_ out) = do
  continue <- isUpdateAvailable =<< readMVar globalRuntimeData
  if not continue
    then do
      hPutStrLn out "Following:"
      mapM_ (hPutStrLn out . showUser) users
      hPutStrLn out "------"
      user <- User <$> getUsername
      eventLoop $ state {_users = users ++ [user]}
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
