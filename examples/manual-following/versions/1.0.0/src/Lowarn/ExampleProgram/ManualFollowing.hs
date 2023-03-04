{-# LANGUAGE RecordWildCards #-}

module Lowarn.ExampleProgram.ManualFollowing
  ( User (..),
    State (..),
    eventLoop,
    showUser,
  )
where

import Lowarn
import System.IO
import Text.Regex.TDFA

newtype User = User
  { userName :: String
  }

data State = State
  { stateUsers :: [User],
    stateIn :: Handle,
    stateOut :: Handle
  }

showUser :: User -> String
showUser = userName

eventLoop :: RuntimeData a -> State -> IO State
eventLoop runtimeData state@State {..} = do
  continue <- isUpdateAvailable runtimeData
  if not continue
    then do
      hPutStrLn stateOut "Following:"
      mapM_ (hPutStrLn stateOut . showUser) stateUsers
      hPutStrLn stateOut "------"
      user <- User <$> getUsername
      eventLoop runtimeData $ state {stateUsers = stateUsers <> [user]}
    else return state
  where
    getUsername :: IO String
    getUsername = do
      hPutStrLn stateOut "Input username of user to follow:"
      hFlush stateOut
      username <- hGetLine stateIn
      if username =~ "\\`[a-zA-Z]+\\'"
        then return username
        else hPutStrLn stateOut "Invalid username, try again." >> getUsername
