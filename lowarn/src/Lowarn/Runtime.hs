{-# LANGUAGE GeneralisedNewtypeDeriving #-}

-- |
-- Module                  : Lowarn.Runtime
-- SPDX-License-Identifier : MIT
-- Stability               : experimental
-- Portability             : non-portable (POSIX, GHC)
--
-- Module for creating and interacting with DSU runtimes.
module Lowarn.Runtime
  ( -- * Runtime creation
    Runtime,
    runRuntime,
    loadProgram,
    liftLinker,
    liftIO,

    -- * Runtime interaction
    Program (..),
    RuntimeData,
    isUpdateAvailable,
    lastState,
  )
where

import Control.Concurrent (MVar, newEmptyMVar, tryPutMVar, tryTakeMVar)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Maybe (isJust)
import Lowarn.DynamicLinker (Linker, liftIO, load, runLinker)
import System.Posix.Signals (Handler (Catch), installHandler, sigUSR2)

newtype UpdateSignal = UpdateSignal
  { unUpdateSignal :: MVar ()
  }

-- | Monad for DSU runtimes that load versions of programs.
newtype Runtime a = Runtime
  { unRuntime :: ReaderT UpdateSignal IO a
  }
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Type for exposing functions that are used by the runtime to load a program.
data Program a b = Program
  { -- | Function to start the program, given data injected by the runtime.
    _entryPoint :: RuntimeData a -> IO a,
    -- | Function to convert old state to new state, if this can be done.
    _transformer :: b -> IO (Maybe a)
  }

-- | Type for accessing data injected by the runtime.
data RuntimeData a = RuntimeData
  { _updateSignal :: UpdateSignal,
    _updateInfo :: Maybe (UpdateInfo a)
  }

data UpdateInfo a = UpdateInfo
  { _label :: Maybe String,
    _lastState :: a
  }

-- | Run a runtime.
runRuntime :: Runtime a -> IO a
runRuntime runtime = do
  updateSignal <- newEmptyMVar
  previousSignalHandler <-
    installHandler
      sigUSR2
      (Catch (void $ tryPutMVar updateSignal ()))
      Nothing
  output <- runReaderT (unRuntime runtime) $ UpdateSignal updateSignal
  liftIO $ void $ installHandler sigUSR2 previousSignalHandler Nothing
  return output

-- | Action that loads and runs a given program, producing the final state of
-- the program when it finishes.
--
-- The program is given as the name of a module that exports a value
-- @program :: 'Program' b a@. This module should be in the package database.
--
-- The program is also given data representing state from the previous version
-- of the program.
loadProgram ::
  -- | The name of the module that includes the program.
  String ->
  -- | State from the previous version of the program.
  a ->
  Runtime b
loadProgram moduleName lastState_ = Runtime $ do
  updateSignal <- ask
  liftIO $
    runLinker (load moduleName "program")
      >>= maybe
        (error ("Loading " <> moduleName <> " failed"))
        ( \program -> do
            updateInfo <-
              fmap (UpdateInfo Nothing)
                <$> _transformer program lastState_
            _entryPoint program $ RuntimeData updateSignal updateInfo
        )

-- | Lift a computation from the 'Linker' monad.
liftLinker :: Linker a -> Runtime a
liftLinker = Runtime . liftIO . runLinker

-- | Return @True@ if the runtime has a program update that can be applied.
isUpdateAvailable :: RuntimeData a -> IO Bool
isUpdateAvailable =
  (fmap isJust . tryTakeMVar) . (unUpdateSignal . _updateSignal)

-- | Return the transformed state of the last version of the program, if there
-- was a previous version of the program and the state was able to be
-- transformed.
lastState :: RuntimeData a -> Maybe a
lastState = fmap _lastState . _updateInfo
