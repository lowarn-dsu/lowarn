{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module                  : Lowarn.Runtime
-- SPDX-License-Identifier : MIT
-- Stability               : experimental
-- Portability             : non-portable (POSIX, GHC)
--
-- Module for interacting with GHC to link modules and load their entities.
module Lowarn.DynamicLinker
  ( Linker,
    runLinker,
    load,
    liftIO,
  )
where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import GHC hiding (load, moduleName, unitState)
import GHC.Data.FastString
import GHC.Driver.Monad
import GHC.Driver.Session hiding (unitState)
import qualified GHC.Driver.Session as Session
import GHC.Driver.Ways
import GHC.Paths (libdir)
import GHC.Runtime.Interpreter
import GHC.Runtime.Linker
import GHC.Types.Name
import GHC.Types.Unique
import GHC.Unit hiding (moduleName)
import System.Environment (lookupEnv)
import Unsafe.Coerce (unsafeCoerce)

-- | Monad for linking modules from the package database and accessing their
-- exported entities.
newtype Linker a = Linker
  { unLinker :: Ghc a
  }
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Run a linker.
runLinker :: Linker a -> IO a
runLinker linker =
  defaultErrorHandler defaultFatalMessager defaultFlushOut $
    runGhc (Just libdir) $ do
      flags <- getSessionDynFlags
      flags' <-
        liftIO $
          lookupEnv "LOWARN_PACKAGE_ENV"
            >>= \case
              Just "" -> return flags
              Nothing -> return flags
              Just lowarnPackageEnv -> do
                interpretPackageEnv $ flags {packageEnv = Just lowarnPackageEnv}
      void $
        setSessionDynFlags $
          addWay' WayDyn $
            flags' {ghcMode = CompManager, ghcLink = LinkDynLib}
      liftIO . initDynLinker =<< getSession
      unLinker linker

-- | Action that gives an entity exported by a module in a package in the
-- package database. The module is linked if it hasn't already been. @Nothing@
-- is given if the module or package cannot be found.
load ::
  -- | The name of the package to find the module in.
  String ->
  -- | The name of the module to find.
  String ->
  -- | The name of the entity to take from the module.
  String ->
  Linker (Maybe a)
load packageName' moduleName' symbol = Linker $ do
  flags <- getSessionDynFlags
  session <- getSession

  let moduleName = mkModuleName moduleName'
      packageName = PackageName $ mkFastString packageName'

  liftIO $
    lookupUnitInfo flags packageName moduleName
      >>= maybe
        (return Nothing)
        ( \unitInfo -> liftIO $ do
            let unit = mkUnit unitInfo
            let module_ = mkModule unit moduleName
            let name =
                  mkExternalName
                    (mkBuiltinUnique 0)
                    module_
                    (mkVarOcc symbol)
                    noSrcSpan

            linkModule session module_

            value <- withInterp session $
              \interp -> getHValue session name >>= wormhole interp
            return $ Just $ unsafeCoerce value
        )

lookupUnitInfo :: DynFlags -> PackageName -> ModuleName -> IO (Maybe UnitInfo)
lookupUnitInfo flags packageName moduleName = do
  case exposedModulesAndPackages of
    [] -> do
      putStrLn $ "Can't find module " <> moduleNameString moduleName
      return Nothing
    (_, unitInfo) : _ -> return $ Just unitInfo
  where
    unitState = Session.unitState flags
    modulesAndPackages = lookupModuleInAllUnits unitState moduleName
    exposedModulesAndPackages =
      filter
        ( \(_, unitInfo) ->
            unitIsExposed unitInfo && unitPackageName unitInfo == packageName
        )
        modulesAndPackages
