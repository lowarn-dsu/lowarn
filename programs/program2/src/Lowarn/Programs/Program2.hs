module Lowarn.Programs.Program2 (program, User (..)) where

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Lowarn.Programs.Program1 as Program1
import Lowarn.Types (Program (..), RuntimeData (..), UpdateInfo (..))
import System.IO (hFlush, stdout)
import System.Random (newStdGen, randomR)
import Text.Printf (printf)
import Text.Read (readMaybe)

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
    ( \(RuntimeData _ maybeUpdateData) ->
        eventLoop $ maybe Seq.empty _lastState maybeUpdateData
    )
    transformer

eventLoop :: Seq User -> IO (Seq User)
eventLoop users = do
  putStrLn "Users:"
  mapM_ print users
  putStrLn "------"
  username <- getInput "Username"
  discriminator <- getDiscriminator
  let user = User username discriminator
  eventLoop $ user Seq.<| users
  where
    getInput :: String -> IO String
    getInput field = do
      putStr $ field <> ": "
      hFlush stdout
      getLine

    getDiscriminator :: IO Int
    getDiscriminator = do
      maybeDiscriminator <- getInput "Discriminator"
      case readMaybe maybeDiscriminator of
        Just discriminator
          | discriminator >= 0 && discriminator <= 9999 -> return discriminator
          | otherwise -> discriminatorError
        Nothing -> discriminatorError
      where
        discriminatorError =
          putStrLn "Invalid discriminator, try again." >> getDiscriminator
