module Lowarn.ExampleProgram.Reproduction
  ( State (..),
    eventLoop,
  )
where

data State = State
  { stateString :: String
  }

eventLoop :: State -> IO State
eventLoop state = do
  putStrLn "State:"
  putStrLn $ stateString state
  putStrLn "------"
  putStrLn "Input a new state or \"update\" to update."
  input <- getLine
  if input == "update"
    then return state
    else do
      putStrLn "======"
      eventLoop $ State input
