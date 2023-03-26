module EntryPoint_following (entryPoint) where

import Data.Maybe (fromMaybe)
import qualified Data.Sequence as Seq
import Foreign (StablePtr, newStablePtr)
import Lowarn (EntryPoint (..), lastState)
import Lowarn.ExampleProgram.Following (State (State), eventLoop)

entryPoint :: EntryPoint State
entryPoint = EntryPoint $
  \runtimeData ->
    eventLoop $ fromMaybe (State Seq.empty) (lastState runtimeData)

foreign export ccall "hs_entryPoint_v2v0v0"
  hsEntryPoint :: IO (StablePtr (EntryPoint State))

hsEntryPoint :: IO (StablePtr (EntryPoint State))
hsEntryPoint = newStablePtr entryPoint
