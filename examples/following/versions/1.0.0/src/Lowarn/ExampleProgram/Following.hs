{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Lowarn.ExampleProgram.Following
  ( User (..),
    State (..),
    eventLoop,
    showUser,
  )
where

import Lowarn
import Lowarn.Inject
import Lowarn.Transformer (deriveGeneric)
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

deriveGeneric ''User
deriveGeneric ''State

showUser :: User -> String
showUser = userName

eventLoop :: State -> IO State
eventLoop state@State {..} = do
  continue <- isUpdateAvailable =<< injectedRuntimeData
  if not continue
    then do
      hPutStrLn stateOut "Following:"
      mapM_ (hPutStrLn stateOut . showUser) stateUsers
      hPutStrLn stateOut "------"
      user <- User <$> getUsername
      eventLoop $ state {stateUsers = stateUsers <> [user]}
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
