module Program2
  ( program,
  )
where

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import System.IO (hFlush, stdout)
import Types (Program (Program), User (..))

program :: Program (Seq User) ()
program = Program eventLoop

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
