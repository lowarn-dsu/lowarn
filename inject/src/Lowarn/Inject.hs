-- |
-- Module                  : Lowarn.Inject
-- SPDX-License-Identifier : MIT
-- Stability               : experimental
-- Portability             : non-portable (POSIX, GHC)
--
-- Module for a plugin used to inject runtime data.
module Lowarn.Inject
  ( InjectRuntimeData,
    injectRuntimeData,
    InjectedRuntimeData,
    injectedRuntimeData,
  )
where

import Lowarn (RuntimeData)

class InjectRuntimeData a where
  injectRuntimeData' :: RuntimeData a -> IO ()

injectRuntimeData :: InjectRuntimeData a => RuntimeData a -> IO ()
injectRuntimeData = injectRuntimeData'

class InjectedRuntimeData a where
  injectedRuntimeData' :: IO (RuntimeData a)

injectedRuntimeData :: InjectedRuntimeData a => IO (RuntimeData a)
injectedRuntimeData = injectedRuntimeData'
