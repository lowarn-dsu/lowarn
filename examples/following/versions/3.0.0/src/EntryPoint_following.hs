module EntryPoint_following (entryPoint) where

import Data.Maybe (fromMaybe)
import Foreign (StablePtr, newStablePtr)
import Lowarn (EntryPoint (..), lastState)
import Lowarn.ExampleProgram.Following (State (State), eventLoop)
import Lowarn.Inject
import System.IO
  ( stdin,
    stdout,
  )

entryPoint :: EntryPoint State
entryPoint = EntryPoint $
  \runtimeData -> do
    injectRuntimeData runtimeData
    eventLoop $ fromMaybe (State [] stdin stdout) $ lastState runtimeData

foreign export ccall "hs_entryPoint_v3v0v0"
  hsEntryPoint :: IO (StablePtr (EntryPoint State))

hsEntryPoint :: IO (StablePtr (EntryPoint State))
hsEntryPoint = newStablePtr entryPoint
