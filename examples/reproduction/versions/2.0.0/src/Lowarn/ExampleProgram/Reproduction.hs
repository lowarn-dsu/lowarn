{-# LANGUAGE DeriveGeneric #-}

module Lowarn.ExampleProgram.Reproduction
  ( State (..),
    eventLoop,
  )
where

import Control.DeepSeq
import Data.Maybe
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import GHC.Generics (Generic)
import System.IO
import Text.Read

data State = State
  { stateInts :: Seq Int,
    stateHandle :: Handle
  }
  deriving (Eq, Generic)

eventLoop :: State -> IO State
eventLoop state = do
  hPutStrLn (stateHandle state) "State:"
  hPrint (stateHandle state) $ fromJust $ Seq.lookup 0 $ stateInts state
  hPrint (stateHandle state) $ Seq.length $ stateInts state
  hPutStrLn (stateHandle state) "------"
  hPutStrLn (stateHandle state) "Input a new state or \"finish\" to finish."
  input <- getLine
  if input == "finish"
    then return state
    else do
      hPutStrLn (stateHandle state) "======"
      eventLoop state {stateInts = (fromMaybe 0 $ readMaybe input) :<| stateInts state}
