{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Lowarn.ExampleProgram.Following
  ( User (..),
    State (..),
    eventLoop,
    showUser,
  )
where

import Control.DeepSeq
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified GHC.Generics as GHC (Generic)
import Lowarn
import Lowarn.Inject
import Lowarn.Transformer (Generic, HasDatatypeInfo)
import System.IO
import Text.Printf
import Text.Regex.TDFA

data User = User
  { userNickname :: String,
    userId :: Int
  }
  deriving (GHC.Generic, NFData, Generic, HasDatatypeInfo)

data State = State
  { stateUsers :: Seq User,
    stateIn :: Handle,
    stateOut :: Handle
  }
  deriving (GHC.Generic, Generic, HasDatatypeInfo)

instance NFData State where
  rnf state = rnf $ stateUsers state

showUser :: User -> String
showUser (User nickname userId) = printf "%s#%04d" nickname userId

eventLoop :: State -> IO State
eventLoop state@State {..} = do
  continue <- isUpdateAvailable =<< injectedRuntimeData
  if not continue
    then do
      hPutStrLn stateOut "Following:"
      mapM_ (hPutStrLn stateOut . showUser) stateUsers
      hPutStrLn stateOut "------"
      userNickname <- getNickname
      userId <- getUserId
      eventLoop $ state {stateUsers = stateUsers Seq.|> User {..}}
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
