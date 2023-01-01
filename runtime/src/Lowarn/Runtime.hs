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
    loadTransformer,
    updatePackageDatabase,
    liftLinker,
    liftIO,
  )
where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Lowarn
  ( EntryPoint (unEntryPoint),
    RuntimeData (RuntimeData),
    Transformer (unTransformer),
    UpdateInfo (UpdateInfo),
    UpdateSignalRegister,
    fillUpdateSignalRegister,
    mkUpdateSignalRegister,
  )
import Lowarn.Linker (Linker, liftIO, load, runLinker)
import qualified Lowarn.Linker as Linker (updatePackageDatabase)
import Lowarn.ProgramName (showEntryPointModuleName, showTransformerModuleName)
import Lowarn.TransformerId (TransformerId, showTransformerPackageName)
import qualified Lowarn.TransformerId as TransformerId (_programName)
import Lowarn.VersionId (VersionId, showVersionPackageName)
import qualified Lowarn.VersionId as VersionId (_programName)
import System.Posix.Signals (Handler (Catch), installHandler, sigUSR2)
import Text.Printf (printf)

-- | Monad for loading versions of programs while handling signals and
-- transferring state.
newtype Runtime a = Runtime
  { unRuntime :: ReaderT UpdateSignalRegister Linker a
  }
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Run a runtime.
runRuntime :: Runtime a -> IO a
runRuntime runtime = do
  updateSignalRegister <- mkUpdateSignalRegister
  previousSignalHandler <-
    installHandler
      sigUSR2
      (Catch (void $ fillUpdateSignalRegister updateSignalRegister))
      Nothing
  output <- runLinker $ runReaderT (unRuntime runtime) updateSignalRegister
  liftIO $ void $ installHandler sigUSR2 previousSignalHandler Nothing
  return output

-- | Lift a computation from the 'Linker' monad.
liftLinker :: Linker a -> Runtime a
liftLinker = Runtime . lift

withLinkedEntity :: String -> String -> String -> (a -> IO b) -> Runtime b
withLinkedEntity packageName moduleName entityName f =
  liftLinker $
    load packageName moduleName entityName
      >>= maybe
        ( error $
            printf
              "Could not find entity %s in module %s in package %s"
              entityName
              moduleName
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
    moduleName
    "hs_entryPoint"
    ( \entryPoint ->
        unEntryPoint entryPoint $
          RuntimeData updateSignalRegister (UpdateInfo <$> mPreviousState)
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
    "hs_transformer"
    (`unTransformer` previousState)
  where
    moduleName =
      showTransformerModuleName . TransformerId._programName $ transformerId
    packageName = showTransformerPackageName transformerId

-- | Action that updates the package database, using
-- 'Linker.updatePackageDatabase'.
updatePackageDatabase :: Runtime ()
updatePackageDatabase = liftLinker Linker.updatePackageDatabase
