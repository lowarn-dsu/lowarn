{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module                  : Lowarn.Runtime
-- SPDX-License-Identifier : MIT
-- Stability               : experimental
-- Portability             : non-portable (POSIX, GHC)
--
-- Module for creating and interacting with DSU runtimes.
module Lowarn.Runtime
  ( Runtime,
    runRuntime,
    loadVersion,
    loadUpdate,
    updateRuntimePackageDatabase,
    liftLinker,
  )
where

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Functor
import Debug.Trace (traceMarkerIO)
import Lowarn
import Lowarn.Linker
import Lowarn.UpdateId
import Lowarn.VersionId
import Lowarn.VersionNumber
import System.Posix.Signals
import Text.Printf

-- | Monad for loading versions of programs while handling signals and
-- transferring state.
newtype Runtime a = Runtime
  { unRuntime :: ReaderT UpdateSignalRegister Linker a
  }
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Run a runtime, optionally unloading code.
runRuntime ::
  -- | A runtime.
  Runtime a ->
  -- | Whether or not to unload code (may result in segmentation faults for lazy
  -- state transformations).
  Bool ->
  -- | Whether or not to use the system linker (rather than the GHC linker).
  Bool ->
  IO a
runRuntime runtime shouldUnload isDynamic = do
  updateSignalRegister <- mkUpdateSignalRegister
  previousSignalHandler <-
    installHandler
      sigUSR2
      (Catch (void $ fillUpdateSignalRegister updateSignalRegister))
      Nothing
  output <-
    runLinker
      (runReaderT (unRuntime runtime) updateSignalRegister)
      shouldUnload
      isDynamic
  liftIO $ void $ installHandler sigUSR2 previousSignalHandler Nothing
  return output

-- | Lift a computation from the 'Linker' monad.
liftLinker :: Linker a -> Runtime a
liftLinker = Runtime . lift

withLinkedEntity ::
  String ->
  String ->
  (a -> IO b) ->
  Runtime b
withLinkedEntity packageName entityReader f =
  liftLinker $
    load packageName entityReader
      >>= maybe
        ( error $
            printf
              "Could not find entities in package %s"
              packageName
        )
        (liftIO . f)

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
  updateSignalRegister <- Runtime ask
  withLinkedEntity
    packageName
    entryPointExport
    $ \entryPoint -> do
      traceMarkerIO $
        printf
          "Version %s begin."
          (showVersionId versionId)
      unEntryPoint entryPoint $
        RuntimeData updateSignalRegister (UpdateInfo <$> mPreviousState)
  where
    packageName = showVersionPackageName versionId
    entryPointExport = showEntryPointExport $ versionIdVersionNumber versionId

-- | Action that performs an given update by running its state transformer,
-- producing the state for the next version of a program, then running the next
-- version of the program with this state.
loadUpdate ::
  -- | The ID corresponding to the update.
  UpdateId ->
  -- | State from the previous version of the program.
  a ->
  Runtime b
loadUpdate updateId@UpdateId {..} previousState = do
  updateSignalRegister <- Runtime ask
  withLinkedEntity
    packageName
    updateExport
    ( \Update {..} -> do
        traceMarkerIO $
          printf "Update %s begin." (showUpdateId updateId)
        transformedPreviousState <-
          evaluate =<< unTransformer updateTransformer previousState
        traceMarkerIO $
          printf "Version %s begin." (showVersionId $ nextVersionId updateId)
        unEntryPoint updateEntryPoint $
          RuntimeData
            updateSignalRegister
            (UpdateInfo <$> transformedPreviousState)
    )
  where
    packageName = showUpdatePackageName updateId
    updateExport =
      showUpdateExport
        updateIdPreviousVersionNumber
        updateIdNextVersionNumber

-- | Action that updates the package database, using
-- 'Linker.updatePackageDatabase'.
updateRuntimePackageDatabase :: Runtime ()
updateRuntimePackageDatabase = liftLinker updatePackageDatabase
