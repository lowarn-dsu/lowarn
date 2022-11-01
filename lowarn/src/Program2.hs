module Program2
  ( main2,
  )
where

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import System.IO (hFlush, stdout)
import Types

main2 :: Program
main2 = Program (\() -> eventLoop2 Seq.empty)

eventLoop2 :: Seq User -> IO ()
eventLoop2 users = do
  putStrLn "Users:"
  mapM_ print users
  putStr "Add user or exit: "
  hFlush stdout
  input <- getLine
  if input == "exit"
    then return ()
    else do
      let user = User input 1234
      eventLoop2 $ user Seq.<| users
