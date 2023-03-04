{-# LANGUAGE RecordWildCards #-}

module Lowarn.ExampleProgram.ManualFollowing
  ( User (..),
    State (..),
    eventLoop,
    showUser,
  )
where

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Lowarn
import System.IO
import Text.Printf
import Text.Regex.TDFA

data User = User
  { userNickname :: String,
    userId :: Int
  }

data State = State
  { stateUsers :: Seq User,
    stateIn :: Handle,
    stateOut :: Handle
  }

showUser :: User -> String
showUser (User nickname userId) = printf "%s#%04d" nickname userId

eventLoop :: RuntimeData a -> State -> IO State
eventLoop runtimeData state@State {..} = do
  continue <- isUpdateAvailable runtimeData
  if not continue
    then do
      hPutStrLn stateOut "Following:"
      mapM_ (hPutStrLn stateOut . showUser) stateUsers
      hPutStrLn stateOut "------"
      userNickname <- getNickname
      userId <- getUserId
      eventLoop runtimeData $ state {stateUsers = stateUsers Seq.|> User {..}}
    else return state
  where
    getInput :: String -> IO String
    getInput field = do
      hPutStrLn stateOut $ printf "Input %s of user to follow:" field
      hFlush stateOut
      hGetLine stateIn

    getNickname :: IO String
    getNickname = do
      nickname <- getInput "nickname"
      if nickname =~ "\\`[a-zA-Z]+\\'"
        then return nickname
        else do
          hPutStrLn stateOut "Invalid nickname, try again."
          getNickname

    getUserId :: IO Int
    getUserId = do
      userId <- getInput "user ID"
      if userId =~ "\\`[0-9]{4}\\'"
        then return $ read userId
        else do
          hPutStrLn stateOut "Invalid user ID, try again."
          getUserId
