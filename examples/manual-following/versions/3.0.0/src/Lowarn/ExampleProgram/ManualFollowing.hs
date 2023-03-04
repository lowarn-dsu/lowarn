{-# LANGUAGE RecordWildCards #-}

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
  { userTag :: String
  }

data State = State
  { stateUsers :: [User],
    stateIn :: Handle,
    stateOut :: Handle
  }

showUser :: User -> String
showUser = userTag

eventLoop :: RuntimeData a -> State -> IO State
eventLoop runtimeData state@State {..} = do
  continue <- isUpdateAvailable runtimeData
  if not continue
    then do
      hPutStrLn stateOut "Following:"
      mapM_ (hPutStrLn stateOut . showUser) $ reverse stateUsers
      hPutStrLn stateOut "------"
      user <- User <$> getTag
      eventLoop runtimeData $ state {stateUsers = user : stateUsers}
    else return state
  where
    getTag :: IO String
    getTag = do
      hPutStrLn stateOut "Input tag of user to follow:"
      hFlush stateOut
      tag <- hGetLine stateIn
      if tag =~ "\\`[a-zA-Z]+#[0-9]{4}\\'"
        then return tag
        else hPutStrLn stateOut "Invalid tag, try again." >> getTag
