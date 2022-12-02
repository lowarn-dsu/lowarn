module Lowarn.Programs.Program2 (program, User (..), State (..)) where

import Control.Arrow (first)
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Lowarn.Programs.Program1 as Program1
import Lowarn.Runtime (Program (..), RuntimeData, isUpdateAvailable, lastState)
import System.IO
  ( Handle,
    hFlush,
    hGetLine,
    hPrint,
    hPutStrLn,
    stdin,
    stdout,
  )
import System.Random (mkStdGen, randomR)
import System.Random.Stateful (applyIOGen, newIOGenM)
import Text.Printf (printf)
import Text.Regex.TDFA

data User = User
  { _nickname :: String,
    _userId :: Int
  }

instance Show User where
  show (User nickname userId) = printf "%s#%04d" nickname userId

data State = State
  { _users :: Seq User,
    _in :: Handle,
    _out :: Handle
  }

transformer :: Program1.State -> IO (Maybe State)
transformer (Program1.State users in_ out) = do
  ioGen <- newIOGenM (mkStdGen 0)
  users' <-
    Seq.fromList
      <$> mapM
        ( \(Program1.User nickname) ->
            applyIOGen
              (first (User nickname) . randomR (1, 9999))
              ioGen
        )
        users
  return $ Just $ State users' in_ out

program :: Program State Program1.State
program =
  Program
    ( \runtimeData ->
        eventLoop runtimeData $
          fromMaybe
            (State Seq.empty stdin stdout)
            (lastState runtimeData)
    )
    transformer

eventLoop :: RuntimeData a -> State -> IO State
eventLoop runtimeData state@(State users in_ out) = do
  continue <- isUpdateAvailable runtimeData
  if not continue
    then do
      hPutStrLn out "Blocked users:"
      mapM_ (hPrint out) users
      hPutStrLn out "------"
      nickname <- getNickname
      userId <- getUserId
      let user = User nickname userId
      eventLoop runtimeData $ state {_users = users Seq.|> user}
    else return state
  where
    getInput :: String -> IO String
    getInput field = do
      hPutStrLn out $ printf "Input %s of user to block:" field
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
