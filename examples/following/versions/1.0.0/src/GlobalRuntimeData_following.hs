module GlobalRuntimeData_following (globalRuntimeData) where

import Control.Concurrent (MVar, newEmptyMVar)
import GHC.IO (unsafePerformIO)
import Lowarn (RuntimeData)
import {-# SOURCE #-} Lowarn.ExampleProgram.Following (State)

{-# NOINLINE globalRuntimeData #-}
globalRuntimeData :: MVar (RuntimeData State)
globalRuntimeData = unsafePerformIO newEmptyMVar
