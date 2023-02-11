{-# LANGUAGE GeneralisedNewtypeDeriving #-}

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
    updatePackageDatabase,
    liftLinker,
    liftIO,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Functor
import Debug.Trace (traceMarkerIO)
import GHC.IO (evaluate)
import Lowarn
  ( EntryPoint (unEntryPoint),
    RuntimeData (RuntimeData),
    Transformer (unTransformer),
    Update (_entryPoint, _transformer),
    UpdateInfo (UpdateInfo),
    UpdateSignalRegister,
    fillUpdateSignalRegister,
    mkUpdateSignalRegister,
  )
import Lowarn.Linker (Linker, liftIO, load, runLinker)
import qualified Lowarn.Linker as Linker (updatePackageDatabase)
import Lowarn.UpdateId
  ( UpdateId (..),
    nextVersionId,
    showUpdateId,
    showUpdatePackageName,
  )
import Lowarn.VersionId
  ( VersionId (_versionNumber),
    showVersionId,
    showVersionPackageName,
  )
import Lowarn.VersionNumber (showEntryPointExport, showUpdateExport)
import System.Posix.Signals (Handler (Catch), installHandler, sigUSR2)
import Text.Printf (printf)

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
  IO a
runRuntime runtime shouldUnload = do
  updateSignalRegister <- mkUpdateSignalRegister
  previousSignalHandler <-
    installHandler
      sigUSR2
      (Catch (void $ fillUpdateSignalRegister updateSignalRegister))
      Nothing
  output <-
    runLinker (runReaderT (unRuntime runtime) updateSignalRegister) shouldUnload
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
    entryPointExport = showEntryPointExport $ _versionNumber versionId

-- | Action that performs an given update by running its state transformer,
-- producing the state for the next version of a program, then running the next
-- version of the program with this state.
loadUpdate ::
  -- | The ID corresponding to the update.
  UpdateId ->
  -- | State from the previous version of the program.
  a ->
  Runtime b
loadUpdate updateId previousState = do
  updateSignalRegister <- Runtime ask
  withLinkedEntity
    packageName
    updateExport
    ( \update -> do
        traceMarkerIO $
          printf
            "Update %s begin."
            (showUpdateId updateId)
        previousState' <-
          evaluate =<< unTransformer (_transformer update) previousState
        traceMarkerIO $
          printf
            "Version %s begin."
            (showVersionId $ nextVersionId updateId)
        unEntryPoint (_entryPoint update) $
          RuntimeData updateSignalRegister (UpdateInfo <$> previousState')
    )
  where
    packageName = showUpdatePackageName updateId
    updateExport =
      showUpdateExport
        (_previousVersionNumber updateId)
        (_nextVersionNumber updateId)

-- | Action that updates the package database, using
-- 'Linker.updatePackageDatabase'.
updatePackageDatabase :: Runtime ()
updatePackageDatabase = liftLinker Linker.updatePackageDatabase
