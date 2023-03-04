module EntryPoint_following (entryPoint) where

import Data.Maybe
import Foreign
import Lowarn
import Lowarn.ExampleProgram.Following
import Lowarn.Inject
import System.IO

entryPoint :: EntryPoint State
entryPoint = EntryPoint $
  \runtimeData -> do
    injectRuntimeData runtimeData
    eventLoop $ fromMaybe (State [] stdin stdout) $ lastState runtimeData

foreign export ccall "hs_entryPoint_v1v0v0"
  hsEntryPoint :: IO (StablePtr (EntryPoint State))

hsEntryPoint :: IO (StablePtr (EntryPoint State))
hsEntryPoint = newStablePtr entryPoint
