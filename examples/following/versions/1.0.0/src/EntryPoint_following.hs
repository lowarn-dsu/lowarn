module EntryPoint_following (entryPoint) where

import Data.Maybe (fromMaybe)
import Foreign (StablePtr, newStablePtr)
import GlobalRuntimeData_following (globalRuntimeData)
import Lowarn (EntryPoint (..), lastState)
import Lowarn.ExampleProgram.Following (State (State), eventLoop)
import Lowarn.GlobalRuntimeData (putGlobalRuntimeData)
import System.IO (stdin, stdout)

entryPoint :: EntryPoint State
entryPoint = EntryPoint $
  \runtimeData -> do
    putGlobalRuntimeData globalRuntimeData runtimeData
    eventLoop $ fromMaybe (State [] stdin stdout) $ lastState runtimeData

foreign export ccall "hs_entryPoint_v1v0v0"
  hsEntryPoint :: IO (StablePtr (EntryPoint State))

hsEntryPoint :: IO (StablePtr (EntryPoint State))
hsEntryPoint = newStablePtr entryPoint
