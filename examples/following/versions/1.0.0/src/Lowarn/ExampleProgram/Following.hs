module Lowarn.ExampleProgram.Following (State (..), eventLoop) where

newtype State = State
  { stateUsers :: [String]
  }

eventLoop :: State -> IO State
eventLoop (State users) = do
  putStrLn "Following (1):"
  mapM_ putStrLn users
  putStrLn "------"
  return $ State ["a"]
