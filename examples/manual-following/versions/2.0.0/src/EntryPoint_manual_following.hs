module EntryPoint_manual_following (entryPoint) where

import Data.Maybe
import qualified Data.Sequence as Seq
import Foreign
import Lowarn
import Lowarn.ExampleProgram.ManualFollowing
import System.IO

entryPoint :: EntryPoint State
entryPoint = EntryPoint $
  \runtimeData ->
    eventLoop runtimeData $
      fromMaybe (State Seq.empty stdin stdout) (lastState runtimeData)

foreign export ccall "hs_entryPoint_v2v0v0"
  hsEntryPoint :: IO (StablePtr (EntryPoint State))

hsEntryPoint :: IO (StablePtr (EntryPoint State))
hsEntryPoint = newStablePtr entryPoint
