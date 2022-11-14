module Lowarn.Programs.Program2 (program, User (..)) where

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Lowarn (isUpdateAvailable)
import qualified Lowarn.Programs.Program1 as Program1
import Lowarn.Types (Program (..), RuntimeData (..), UpdateInfo (..))
import System.IO (hFlush, stdout)
import System.Random (newStdGen, randomR)
import Text.Printf (printf)
import Text.Regex.TDFA

data User = User
  { _username :: String,
    _discriminator :: Int
  }

instance Show User where
  show (User username discriminator) = printf "%s#%04d" username discriminator

transformer :: [Program1.User] -> IO (Maybe (Seq User))
transformer =
  fmap (Just . Seq.fromList)
    . mapM
      ( \(Program1.User username) ->
          User username . fst . randomR (1, 9999) <$> newStdGen
      )

program :: Program (Seq User) [Program1.User]
program =
  Program
    ( \runtimeData ->
        eventLoop runtimeData $
          maybe Seq.empty _lastState (_updateInfo runtimeData)
    )
    transformer

eventLoop :: RuntimeData a -> Seq User -> IO (Seq User)
eventLoop runtimeData users = do
  continue <- isUpdateAvailable runtimeData
  if not continue
    then do
      putStrLn "Users:"
      mapM_ print users
      putStrLn "------"
      username <- getUsername
      discriminator <- getDiscriminator
      let user = User username discriminator
      eventLoop runtimeData $ user Seq.<| users
    else return users
  where
    getInput :: String -> IO String
    getInput field = do
      putStr $ field <> ": "
      hFlush stdout
      getLine

    getUsername :: IO String
    getUsername = do
      username <- getInput "Username"
      if username =~ "\\`[a-zA-Z]+\\'"
        then return username
        else putStrLn "Invalid username, try again." >> getUsername

    getDiscriminator :: IO Int
    getDiscriminator = do
      discriminator <- getInput "Discriminator"
      if discriminator =~ "\\`[0-9]{4}\\'"
        then return $ read discriminator
        else putStrLn "Invalid discriminator, try again." >> getDiscriminator
