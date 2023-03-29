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

import Lowarn

-- | A typeclass that allows @'RuntimeData' a@ to be set in certain modules.
-- Instances of this typeclass are generated by the injection plugin in modules
-- with the name @EntryPoint_program-name@, where @program-name@ is the name of
-- the program that the plugin is being used in.
class InjectRuntimeData a where
  injectRuntimeData' :: RuntimeData a -> IO ()

-- | Set the 'RuntimeData' associated with a version of a program, when we have
-- an instance of @'InjectRuntimeData' a@.
injectRuntimeData :: (InjectRuntimeData a) => RuntimeData a -> IO ()
injectRuntimeData = injectRuntimeData'

-- | A typeclass that allows @'RuntimeData' a@ to be accessed in certain
-- modules. Instances of this typeclass are generated by the injection plugin.
class InjectedRuntimeData a where
  injectedRuntimeData' :: IO (RuntimeData a)

-- | Get the 'RuntimeData' associated with a version of a program, when we have
-- an instance of @'InjectedRuntimeData' a@.
injectedRuntimeData :: (InjectedRuntimeData a) => IO (RuntimeData a)
injectedRuntimeData = injectedRuntimeData'