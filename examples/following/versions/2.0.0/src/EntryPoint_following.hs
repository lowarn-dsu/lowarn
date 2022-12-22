module EntryPoint_following
  ( entryPoint,
  )
where

import Data.Maybe (fromMaybe)
import qualified Data.Sequence as Seq
import Lowarn.ExampleProgram.Following (State (State), eventLoop)
import Lowarn.Runtime (EntryPoint (..), lastState)
import System.IO
  ( stdin,
    stdout,
  )

entryPoint :: EntryPoint State
entryPoint = EntryPoint $
  \runtimeData ->
    eventLoop runtimeData $
      fromMaybe (State Seq.empty stdin stdout) (lastState runtimeData)
