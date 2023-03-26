module Lowarn.ExampleProgram.Reproduction
  ( State (..),
    eventLoop,
  )
where

import System.IO

data State = State
  { stateStrings :: [String],
    stateHandle :: Handle
  }

addQuotes :: String -> String
addQuotes s = '"' : s ++ "\""

eventLoop :: State -> IO State
eventLoop state = do
  hPutStrLn (stateHandle state) "State:"
  hPutStrLn (stateHandle state) $ head $ stateStrings state
  hPrint (stateHandle state) $ length $ stateStrings state
  hPutStrLn (stateHandle state) "------"
  hPutStrLn (stateHandle state) "Input a new state or \"update\" to update."
  input <- getLine
  if input == "update"
    then return state
    else do
      hPutStrLn (stateHandle state) "======"
      eventLoop state {stateStrings = addQuotes input ++ stateStrings state}
