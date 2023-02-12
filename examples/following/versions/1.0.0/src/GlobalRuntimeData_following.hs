module GlobalRuntimeData_following (globalRuntimeData) where

import GHC.IO (unsafePerformIO)
import {-# SOURCE #-} Lowarn.ExampleProgram.Following (State)
import Lowarn.GlobalRuntimeData (GlobalRuntimeData, newGlobalRuntimeData)

{-# NOINLINE globalRuntimeData #-}
globalRuntimeData :: GlobalRuntimeData (State)
globalRuntimeData = unsafePerformIO newGlobalRuntimeData
