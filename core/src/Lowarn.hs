-- |
-- Module                  : Lowarn
-- SPDX-License-Identifier : MIT
-- Stability               : experimental
-- Portability             : portable
--
-- Module for interacting with Lowarn.
module Lowarn
  ( -- * Versions

    -- | Types and functions used in versions of programs.
    EntryPoint (..),
    isUpdateAvailable,
    signalUpdate,
    lastState,

    -- * Transformers

    -- | Types and functions used in state transformers.
    Transformer (..),

    -- * Runtime

    -- | Types and functions used by Lowarn's runtime.
    RuntimeData (RuntimeData),
    UpdateSignalRegister,
    mkUpdateSignalRegister,
    fillUpdateSignalRegister,
    UpdateInfo (UpdateInfo),
  )
where

import Control.Concurrent (MVar, newEmptyMVar, tryPutMVar, tryTakeMVar)
import Data.Maybe (isJust)

-- | Type for storing whether or not Lowarn should try to update a program.
newtype UpdateSignalRegister = UpdateSignalRegister
  { unUpdateSignalRegister :: MVar ()
  }

-- | Type for information about a successful update from one version of a
-- program to another.
newtype UpdateInfo a = UpdateInfo
  { -- | The state of the previous version of the program, after being
    -- transformed.
    _lastState :: a
  }

-- | Type for accessing data injected by the runtime.
data RuntimeData a = RuntimeData
  { -- | An update signal register.
    _updateSignalRegister :: UpdateSignalRegister,
    -- | If an update has occurred successfully, information about the update.
    _updateInfo :: Maybe (UpdateInfo a)
  }

-- | Type for functions that begin running a version of a program.
newtype EntryPoint a = EntryPoint
  { unEntryPoint :: RuntimeData a -> IO a
  }

-- | Type for functions that transform state from one version of a program
-- into state for another.
newtype Transformer a b = Transformer
  { unTransformer :: a -> IO (Maybe b)
  }

-- | Create an update signal register.
mkUpdateSignalRegister :: IO UpdateSignalRegister
mkUpdateSignalRegister = UpdateSignalRegister <$> newEmptyMVar

-- | Fill the update signal register. A boolean is returned which is @False@ if
-- the register has already been filled, and @True@ if it hasn't.
fillUpdateSignalRegister :: UpdateSignalRegister -> IO Bool
fillUpdateSignalRegister = (`tryPutMVar` ()) . unUpdateSignalRegister

-- | Return @True@ if the runtime has a program update that can be applied.
isUpdateAvailable :: RuntimeData a -> IO Bool
isUpdateAvailable =
  (fmap isJust . tryTakeMVar) . (unUpdateSignalRegister . _updateSignalRegister)

-- | Send a signal to Lowarn that indicates that an update is available. A
-- boolean is returned which is @False@ if an update has already been signalled,
-- and @True@ if one hasn't.
signalUpdate :: RuntimeData a -> IO Bool
signalUpdate = fillUpdateSignalRegister . _updateSignalRegister

-- | Return the transformed state of the last version of the program, if there
-- was a previous version of the program and the state was able to be
-- transformed.
lastState :: RuntimeData a -> Maybe a
lastState = fmap _lastState . _updateInfo
