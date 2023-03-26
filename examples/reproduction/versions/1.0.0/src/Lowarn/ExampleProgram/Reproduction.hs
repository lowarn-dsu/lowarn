module Lowarn.ExampleProgram.Reproduction (State (..), eventLoop) where

newtype State = State
  { unState :: String
  }

eventLoop :: State -> IO State
eventLoop state = do
  putStrLn "State (1):"
  putStrLn $ unState state
  putStrLn "------"
  return $ State "a"
