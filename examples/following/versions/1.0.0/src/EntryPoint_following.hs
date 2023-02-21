{-# OPTIONS_GHC -dcore-lint #-}

module EntryPoint_following (entryPoint, runtimeDataVar) where

import Data.Maybe (fromMaybe)
import Foreign (StablePtr, newStablePtr)
import GHC.IO (unsafePerformIO)
import Lowarn (EntryPoint (..), lastState)
import {-# SOURCE #-} Lowarn.ExampleProgram.Following (State (State), eventLoop)
import Lowarn.Inject
import Lowarn.Inject.RuntimeDataVar (RuntimeDataVar, newRuntimeDataVar)
import System.IO (stdin, stdout)

entryPoint :: EntryPoint State
entryPoint = EntryPoint $
  \runtimeData -> do
    injectRuntimeData runtimeData
    eventLoop $ fromMaybe (State [] stdin stdout) $ lastState runtimeData

foreign export ccall "hs_entryPoint_v1v0v0"
  hsEntryPoint :: IO (StablePtr (EntryPoint State))

hsEntryPoint :: IO (StablePtr (EntryPoint State))
hsEntryPoint = newStablePtr entryPoint

{-# NOINLINE runtimeDataVar #-}
runtimeDataVar :: RuntimeDataVar (State)
runtimeDataVar = unsafePerformIO newRuntimeDataVar
