module EntryPoint_following (entryPoint) where

import Data.Maybe
import Foreign
import Lowarn
import Lowarn.ExampleProgram.Reproduction

entryPoint :: EntryPoint State
entryPoint = EntryPoint $
  \runtimeData ->
    eventLoop $ fromMaybe (State 0) (lastState runtimeData)

foreign export ccall "hs_entryPoint_v2v0v0"
  hsEntryPoint :: IO (StablePtr (EntryPoint State))

hsEntryPoint :: IO (StablePtr (EntryPoint State))
hsEntryPoint = newStablePtr entryPoint
