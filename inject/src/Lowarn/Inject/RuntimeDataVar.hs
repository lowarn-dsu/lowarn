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

import Control.Concurrent (MVar, newEmptyMVar, putMVar, readMVar)
import Lowarn (RuntimeData)

newtype RuntimeDataVar a = RuntimeDataVar
  { unRuntimeDataVar :: MVar (RuntimeData a)
  }

newRuntimeDataVar :: IO (RuntimeDataVar a)
newRuntimeDataVar = RuntimeDataVar <$> newEmptyMVar

putRuntimeDataVar :: RuntimeDataVar a -> RuntimeData a -> IO ()
putRuntimeDataVar = putMVar . unRuntimeDataVar

readRuntimeDataVar :: RuntimeDataVar a -> IO (RuntimeData a)
readRuntimeDataVar = readMVar . unRuntimeDataVar
