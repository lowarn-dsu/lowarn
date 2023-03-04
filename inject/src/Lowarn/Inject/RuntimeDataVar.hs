-- |
-- Module                  : Lowarn.Inject.RuntimeDataVar
-- SPDX-License-Identifier : MIT
-- Stability               : experimental
-- Portability             : non-portable (GHC)
--
-- Module for a variable that stores runtime data.
module Lowarn.Inject.RuntimeDataVar
  ( RuntimeDataVar,
    newRuntimeDataVar,
    putRuntimeDataVar,
    readRuntimeDataVar,
  )
where

import Control.Concurrent.MVar
import Control.Exception
import Lowarn

-- | A variable that stores the runtime data associated with a version of a
-- program.
newtype RuntimeDataVar a = RuntimeDataVar
  { unRuntimeDataVar :: MVar (RuntimeData a)
  }

-- | Create a @'RuntimeDataVar' a@ that has not been set yet.
newRuntimeDataVar :: IO (RuntimeDataVar a)
newRuntimeDataVar = RuntimeDataVar <$> newEmptyMVar

-- | Set a @'RuntimeDataVar' a@'s runtime data. This action will raise an
-- 'IOException' if it is run more than once on the same @'RuntimeDataVar' a@.
putRuntimeDataVar :: RuntimeDataVar a -> RuntimeData a -> IO ()
putRuntimeDataVar runtimeDataVar runtimeData = do
  mVarWasNotEmpty <- tryPutMVar (unRuntimeDataVar runtimeDataVar) runtimeData
  if mVarWasNotEmpty
    then return ()
    else throw $ userError "RuntimeDataVar set more than once."

-- | Get a @'RuntimeDataVar' a@'s runtime data. This action will wait for the
-- variable to be set if it hasn't already been.
readRuntimeDataVar :: RuntimeDataVar a -> IO (RuntimeData a)
readRuntimeDataVar = readMVar . unRuntimeDataVar
