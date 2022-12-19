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
    loadVersion,
    loadTransformer,
    updatePackageDatabase,
    liftLinker,
    liftIO,

    -- * Versions

    -- | Types and functions used in versions of programs.
    EntryPoint (..),
    RuntimeData,
    isUpdateAvailable,
    lastState,

    -- * Transformers

    -- | Types and functions used in state transformers.
    Transformer (..),
  )
where

import Control.Concurrent (MVar, newEmptyMVar, tryPutMVar, tryTakeMVar)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Maybe (isJust)
import Lowarn.DynamicLinker (Linker, liftIO, load, runLinker)
import Lowarn.ProgramName (showEntryPointModuleName, showTransformerModuleName)
import Lowarn.TransformerId (TransformerId, showTransformerPackageName)
import qualified Lowarn.TransformerId as TransformerId (_programName)
import Lowarn.VersionId (VersionId, showVersionPackageName)
import qualified Lowarn.VersionId as VersionId (_programName)
import System.Posix.Signals (Handler (Catch), installHandler, sigUSR2)
import Text.Printf (printf)

newtype UpdateSignal = UpdateSignal
  { unUpdateSignal :: MVar ()
  }

-- | Monad for loading versions of programs while handling signals and
-- transferring state.
newtype Runtime a = Runtime
  { unRuntime :: ReaderT UpdateSignal IO a
  }
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Type for functions that begin running a version of a program.
newtype EntryPoint a = EntryPoint
  { unEntryPoint :: RuntimeData a -> IO a
  }

-- | Type for functions that transform state from one version of a program
-- into state for another.
newtype Transformer a b = Transformer
  { unTransformer :: a -> IO (Maybe b)
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

withLinkedEntity :: String -> String -> String -> (a -> IO b) -> Runtime b
withLinkedEntity packageName moduleName entityName f =
  liftIO $ do
    runLinker (load packageName moduleName entityName)
      >>= maybe
        ( error
            ( printf
                "Could not find entity %s in module %s in package %s"
                entityName
                moduleName
                packageName
            )
        )
        f

-- | Action that loads and runs a given version of a program, producing the
-- final state of the program when it finishes.
--
-- The program can be given data representing state transformed from a previous
-- version of the program.
loadVersion ::
  -- | The ID corresponding to the version of the program.
  VersionId ->
  -- | State from a previous version of the program after being transformed.
  Maybe a ->
  Runtime a
loadVersion versionId mPreviousState = do
  updateSignal <- Runtime ask
  withLinkedEntity
    packageName
    moduleName
    "entryPoint"
    ( \entryPoint ->
        unEntryPoint entryPoint $
          RuntimeData updateSignal (UpdateInfo Nothing <$> mPreviousState)
    )
  where
    moduleName =
      showEntryPointModuleName . VersionId._programName $ versionId
    packageName = showVersionPackageName versionId

-- | Action that loads and runs a given state transformer, producing the state
-- for the next version of a program.
loadTransformer ::
  -- | The ID corresponding to the transformer.
  TransformerId ->
  -- | State from the previous version of the program.
  a ->
  Runtime (Maybe a)
loadTransformer transformerId previousState =
  withLinkedEntity
    packageName
    moduleName
    "transformer"
    (`unTransformer` previousState)
  where
    moduleName =
      showTransformerModuleName . TransformerId._programName $ transformerId
    packageName = showTransformerPackageName transformerId

-- | Action that updates the package database.
updatePackageDatabase :: Runtime ()
updatePackageDatabase = return ()

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
