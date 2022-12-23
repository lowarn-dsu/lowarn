module EntryPoint_following
  ( entryPoint,
  )
where

import Data.Maybe (fromMaybe)
import Foreign (StablePtr, newStablePtr)
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
      fromMaybe (State [] stdin stdout) (lastState runtimeData)

foreign export ccall "hs_entryPoint"
  hsEntryPoint :: IO (StablePtr (EntryPoint State))

hsEntryPoint :: IO (StablePtr (EntryPoint State))
hsEntryPoint = newStablePtr entryPoint
