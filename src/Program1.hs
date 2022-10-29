module Program1
  ( main1,
  )
where

import qualified Control.Monad
import System.IO (hFlush, stdout)
import Types

main1 :: Program
main1 = Program (\() -> Control.Monad.void (eventLoop1 []))

eventLoop1 :: [User] -> IO [User]
eventLoop1 users = do
  putStrLn "Users:"
  mapM_ print users
  putStr "Add user or exit: "
  hFlush stdout
  input <- getLine
  if input == "exit"
    then return users
    else do
      let user = User input 1234
      eventLoop1 $ user : users
