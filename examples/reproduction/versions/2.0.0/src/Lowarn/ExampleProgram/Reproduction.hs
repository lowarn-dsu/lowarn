module Lowarn.ExampleProgram.Reproduction (State (..), eventLoop) where

newtype State = State
  { unState :: String
  }

eventLoop :: State -> IO State
eventLoop (State innerState) = do
  putStrLn "State (2):"
  putStrLn innerState
  putStrLn "------"
  input <- getLine
  eventLoop $ State (innerState ++ input)
