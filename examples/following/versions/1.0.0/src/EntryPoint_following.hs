module EntryPoint_following
  ( entryPoint,
  )
where

import Data.Maybe (fromMaybe)
import Lowarn (EntryPoint (..), lastState)
import Lowarn.ExampleProgram.Following (State (State), eventLoop)
import System.IO
  ( stdin,
    stdout,
  )

entryPoint :: EntryPoint State
entryPoint = EntryPoint $
  \runtimeData ->
    eventLoop runtimeData $
      fromMaybe (State [] stdin stdout) (lastState runtimeData)
