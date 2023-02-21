module RuntimeDataVar_following (runtimeDataVar) where

import GHC.IO (unsafePerformIO)
import {-# SOURCE #-} Lowarn.ExampleProgram.Following (State)
import Lowarn.Inject.RuntimeDataVar (RuntimeDataVar, newRuntimeDataVar)

{-# NOINLINE runtimeDataVar #-}
runtimeDataVar :: RuntimeDataVar State
runtimeDataVar = unsafePerformIO newRuntimeDataVar
