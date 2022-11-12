module Program1
  ( program,
  )
where

import qualified Control.Monad
import System.IO (hFlush, stdout)
import Types (Program (Program), User (..))

program :: Program
program = Program (\() -> Control.Monad.void (eventLoop []))

eventLoop :: [User] -> IO [User]
eventLoop users = do
  putStrLn "Users:"
  mapM_ print users
  putStr "Add user or exit: "
  hFlush stdout
  input <- getLine
  if input == "exit"
    then return users
    else do
      let user = User input 1234
      eventLoop $ user : users
