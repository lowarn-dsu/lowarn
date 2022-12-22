module Lowarn.ExampleProgram.Following
  ( User (..),
    State (..),
    eventLoop,
    showUser,
  )
where

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Lowarn.Runtime (RuntimeData, isUpdateAvailable)
import System.IO
  ( Handle,
    hFlush,
    hGetLine,
    hPutStrLn,
  )
import Text.Printf (printf)
import Text.Regex.TDFA

data User = User
  { _nickname :: String,
    _userId :: Int
  }

data State = State
  { _users :: Seq User,
    _in :: Handle,
    _out :: Handle
  }

showUser :: User -> String
showUser (User nickname userId) = printf "%s#%04d" nickname userId

eventLoop :: RuntimeData a -> State -> IO State
eventLoop runtimeData state@(State users in_ out) = do
  continue <- isUpdateAvailable runtimeData
  if not continue
    then do
      hPutStrLn out "Following:"
      mapM_ (hPutStrLn out . showUser) users
      hPutStrLn out "------"
      nickname <- getNickname
      userId <- getUserId
      let user = User nickname userId
      eventLoop runtimeData $ state {_users = users Seq.|> user}
    else return state
  where
    getInput :: String -> IO String
    getInput field = do
      hPutStrLn out $ printf "Input %s of user to follow:" field
      hFlush out
      hGetLine in_

    getNickname :: IO String
    getNickname = do
      nickname <- getInput "nickname"
      if nickname =~ "\\`[a-zA-Z]+\\'"
        then return nickname
        else do
          hPutStrLn out "Invalid nickname, try again."
          getNickname

    getUserId :: IO Int
    getUserId = do
      userId <- getInput "user ID"
      if userId =~ "\\`[0-9]{4}\\'"
        then return $ read userId
        else do
          hPutStrLn out "Invalid user ID, try again."
          getUserId
