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
    loadTransformerAndVersion,
    updatePackageDatabase,
    liftLinker,
    liftIO,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Functor
import GHC.IO (evaluate)
import Lowarn
  ( EntryPoint (unEntryPoint),
    RuntimeData (RuntimeData),
    Transformer (unTransformer),
    UpdateInfo (UpdateInfo),
    UpdateSignalRegister,
    fillUpdateSignalRegister,
    mkUpdateSignalRegister,
  )
import Lowarn.Linker
  ( Entity (..),
    GetEntityProgram,
    Linker,
    getEntity,
    liftIO,
    load,
    runLinker,
  )
import qualified Lowarn.Linker as Linker (updatePackageDatabase)
import Lowarn.ProgramName (showEntryPointModuleName, showTransformerModuleName)
import Lowarn.TransformerId
  ( TransformerId (_nextVersionNumber, _previousVersionNumber),
    showTransformerPackageName,
  )
import qualified Lowarn.TransformerId as TransformerId (_programName)
import Lowarn.VersionId (VersionId (_versionNumber), showVersionPackageName)
import qualified Lowarn.VersionId as VersionId (_programName)
import Lowarn.VersionNumber (showEntryPointExport, showTransformerExport)
import System.Posix.Signals (Handler (Catch), installHandler, sigUSR2)
import Text.Printf (printf)
import Unsafe.Coerce (unsafeCoerce)

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

withLinkedEntity ::
  String -> String -> String -> (a -> IO b) -> Runtime b
withLinkedEntity packageName moduleName entityName f =
  liftLinker $
    load packageName moduleName getEntityProgram
      >>= maybe
        ( error $
            printf
              "Could not find entity %s in module %s in package %s"
              entityName
              moduleName
              packageName
        )
        (liftIO . f)
  where
    getEntityProgram :: GetEntityProgram (Maybe a)
    getEntityProgram =
      getEntity entityName
        <&> maybe Nothing (\(Entity a) -> Just $ unsafeCoerce a)

withLinkedEntity' ::
  String -> String -> String -> String -> ((a, b) -> IO c) -> Runtime c
withLinkedEntity' packageName moduleName entityName1 entityName2 f =
  liftLinker $
    load packageName moduleName getEntityProgram
      >>= maybe
        ( error $
            printf
              "Could not find entities %s and %s in module %s in package %s"
              entityName1
              entityName2
              moduleName
              packageName
        )
        (liftIO . f)
  where
    getEntityProgram :: GetEntityProgram (Maybe (a, b))
    getEntityProgram =
      getEntity entityName1
        >>= maybe
          (return Nothing)
          ( \(Entity a) ->
              getEntity entityName2
                <&> maybe Nothing (\(Entity b) -> Just $ unsafeCoerce (a, b))
          )

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
    (showEntryPointExport $ _versionNumber versionId)
    $ \entryPoint ->
      unEntryPoint entryPoint $
        RuntimeData updateSignalRegister (UpdateInfo <$> mPreviousState)
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
  Runtime (Maybe b)
loadTransformer transformerId previousState =
  withLinkedEntity
    packageName
    moduleName
    ( showTransformerExport
        (_previousVersionNumber transformerId)
        (_nextVersionNumber transformerId)
    )
    (`unTransformer` previousState)
  where
    moduleName =
      showTransformerModuleName . TransformerId._programName $ transformerId
    packageName = showTransformerPackageName transformerId

-- | Action that loads and runs a given state transformer, producing the state
-- for the next version of a program.
loadTransformerAndVersion ::
  -- | The ID corresponding to the transformer.
  TransformerId ->
  -- | The ID corresponding to the version.
  VersionId ->
  -- | State from the previous version of the program.
  a ->
  Runtime b
loadTransformerAndVersion transformerId versionId previousState = do
  updateSignalRegister <- Runtime ask
  withLinkedEntity'
    packageName
    moduleName
    ( showTransformerExport
        (_previousVersionNumber transformerId)
        (_nextVersionNumber transformerId)
    )
    (showEntryPointExport $ _versionNumber versionId)
    ( \(transformer, entryPoint) -> do
        previousState' <-
          evaluate =<< unTransformer transformer previousState
        unEntryPoint entryPoint $
          RuntimeData updateSignalRegister (UpdateInfo <$> previousState')
    )
  where
    moduleName =
      showTransformerModuleName . TransformerId._programName $ transformerId
    packageName = showTransformerPackageName transformerId

-- | Action that updates the package database, using
-- 'Linker.updatePackageDatabase'.
updatePackageDatabase :: Runtime ()
updatePackageDatabase = liftLinker Linker.updatePackageDatabase
