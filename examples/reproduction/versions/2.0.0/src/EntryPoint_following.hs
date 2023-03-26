module EntryPoint_following (entryPoint) where

import Data.Maybe
import qualified Data.Sequence as Seq
import Foreign
import Lowarn
import Lowarn.ExampleProgram.Reproduction
import System.IO

entryPoint :: EntryPoint State
entryPoint = EntryPoint $
  \runtimeData ->
    eventLoop $ fromMaybe (State (Seq.singleton 0) stdout) (lastState runtimeData)

foreign export ccall "hs_entryPoint_v2v0v0"
  hsEntryPoint :: IO (StablePtr (EntryPoint State))

hsEntryPoint :: IO (StablePtr (EntryPoint State))
hsEntryPoint = newStablePtr entryPoint
