{-# LANGUAGE DeriveGeneric #-}

module Lowarn.ExampleProgram.Reproduction
  ( State (..),
    eventLoop,
  )
where

import Control.DeepSeq
import Data.Maybe
import GHC.Generics (Generic)
import Text.Read

data State = State
  { stateInt :: Int
  }
  deriving (Eq, Generic)

instance NFData State

eventLoop :: State -> IO State
eventLoop state = do
  putStrLn "State:"
  print $ stateInt state
  putStrLn "------"
  putStrLn "Input a new state or \"finish\" to finish."
  input <- getLine
  if input == "finish"
    then return state
    else do
      putStrLn "======"
      eventLoop $ State $ fromMaybe 0 $ readMaybe input
