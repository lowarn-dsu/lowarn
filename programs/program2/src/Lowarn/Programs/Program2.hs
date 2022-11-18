module Lowarn.Programs.Program2 (program, User (..), State (..)) where

import Control.Arrow (first)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Lowarn (isUpdateAvailable)
import qualified Lowarn.Programs.Program1 as Program1
import Lowarn.Types (Program (..), RuntimeData (..), UpdateInfo (..))
import System.IO
  ( Handle,
    hFlush,
    hGetLine,
    hPrint,
    hPutStr,
    hPutStrLn,
    stdin,
    stdout,
  )
import System.Random (mkStdGen, randomR)
import System.Random.Stateful (applyIOGen, newIOGenM)
import Text.Printf (printf)
import Text.Regex.TDFA

data User = User
  { _username :: String,
    _discriminator :: Int
  }

instance Show User where
  show (User username discriminator) = printf "%s#%04d" username discriminator

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
        ( \(Program1.User username) ->
            applyIOGen
              (first (User username) . randomR (1, 9999))
              ioGen
        )
        users
  return $ Just $ State users' in_ out

program :: Program State Program1.State
program =
  Program
    ( \runtimeData ->
        eventLoop runtimeData $
          maybe
            (State Seq.empty stdin stdout)
            _lastState
            (_updateInfo runtimeData)
    )
    transformer

eventLoop :: RuntimeData a -> State -> IO State
eventLoop runtimeData state@(State users in_ out) = do
  continue <- isUpdateAvailable runtimeData
  if not continue
    then do
      hPutStrLn out "Users:"
      mapM_ (hPrint out) users
      hPutStrLn out "------"
      username <- getUsername
      discriminator <- getDiscriminator
      let user = User username discriminator
      eventLoop runtimeData $ state {_users = user Seq.<| users}
    else return state
  where
    getInput :: String -> IO String
    getInput field = do
      hPutStr out $ field <> ": "
      hFlush out
      hGetLine in_

    getUsername :: IO String
    getUsername = do
      username <- getInput "Username"
      if username =~ "\\`[a-zA-Z]+\\'"
        then return username
        else do
          hPutStrLn out "Invalid username, try again."
          getUsername

    getDiscriminator :: IO Int
    getDiscriminator = do
      discriminator <- getInput "Discriminator"
      if discriminator =~ "\\`[0-9]{4}\\'"
        then return $ read discriminator
        else do
          hPutStrLn out "Invalid discriminator, try again."
          getDiscriminator
