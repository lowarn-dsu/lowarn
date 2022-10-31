{-# LANGUAGE LambdaCase #-}

module DynamicLinker (load) where

import DynFlags (DynFlags, defaultFatalMessager, defaultFlushOut)
import GHC (defaultErrorHandler, getSession, getSessionDynFlags, mkModule, mkModuleName, runGhc, setSessionDynFlags)
import GHC.Paths (libdir)
import GhcMonad (liftIO)
import Linker (getHValue, linkModule)
import Module (UnitId)
import Name (mkExternalName)
import OccName (mkVarOcc)
import PackageConfig (packageConfigId)
import qualified PackageConfig
import Packages (lookupModuleInAllPackages)
import SrcLoc (noSrcSpan)
import Unique (mkBuiltinUnique)
import Unsafe.Coerce (unsafeCoerce)

load ::
  String ->
  String ->
  IO (Maybe a)
load moduleName symbol = do
  defaultErrorHandler defaultFatalMessager defaultFlushOut $
    runGhc (Just libdir) $ do
      _linkerPackages <- setSessionDynFlags =<< getSessionDynFlags
      flags <- getSessionDynFlags

      liftIO (lookupUnitId flags moduleName) >>= \case
        Nothing -> return Nothing
        Just unitId -> do
          let module_ =
                mkModule
                  unitId
                  (mkModuleName moduleName)

          session <- getSession

          liftIO $ linkModule session module_

          let name =
                mkExternalName
                  (mkBuiltinUnique 0)
                  module_
                  (mkVarOcc symbol)
                  noSrcSpan

          value <- liftIO $ getHValue session name
          return $ Just $ unsafeCoerce value

lookupUnitId :: DynFlags -> String -> IO (Maybe UnitId)
lookupUnitId flags moduleName = case exposedModulesAndPackages of
  [] -> do
    putStrLn $ "Can't find module " <> show moduleName
    return Nothing
  (_, packageConfig) : _ ->
    return $ Just $ packageConfigId packageConfig
  where
    modulesAndPackages =
      lookupModuleInAllPackages flags (mkModuleName moduleName)
    exposedModulesAndPackages =
      filter (\(_, packageConfig) -> PackageConfig.exposed packageConfig) modulesAndPackages
