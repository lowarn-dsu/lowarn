module Lowarn.Programs.Program2
  ( program,
    User (..),
  )
where

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Lowarn.Programs.Program1 as Program1
import Lowarn.Types (Program (Program))
import System.IO (hFlush, stdout)

data User = User
  { username :: String,
    discriminator :: Int
  }
  deriving (Show)

transformer :: [Program1.User] -> Seq User
transformer = Seq.fromList . map (\(Program1.User username' discriminator') -> User username' discriminator')

program :: Program (Seq User) () [Program1.User]
program = Program eventLoop transformer

eventLoop :: Seq User -> IO ()
eventLoop users = do
  putStrLn "Users:"
  mapM_ print users
  putStr "Add user or exit: "
  hFlush stdout
  input <- getLine
  if input == "exit"
    then return ()
    else do
      let user = User input 1234
      eventLoop $ user Seq.<| users
