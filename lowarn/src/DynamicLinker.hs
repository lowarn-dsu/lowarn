{-# LANGUAGE LambdaCase #-}

module DynamicLinker (load) where

import GHC hiding (load, moduleName, unitState)
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
import qualified GHC.Unit as Unit
import Unsafe.Coerce (unsafeCoerce)

load ::
  String ->
  String ->
  IO (Maybe a)
load moduleName' symbol = do
  defaultErrorHandler defaultFatalMessager defaultFlushOut $
    runGhc (Just libdir) $ do
      flags' <- getSessionDynFlags
      _ <- setSessionDynFlags $ addWay' WayDyn $ flags' {ghcMode = CompManager, ghcLink = LinkDynLib}
      flags <- getSessionDynFlags

      session <- getSession
      liftIO $ initDynLinker session

      let moduleName = mkModuleName moduleName'

      liftIO (lookupUnitInfo flags moduleName) >>= \case
        Nothing -> return Nothing
        Just unitInfo -> do
          liftIO $ linkPackages session [Unit.unitId unitInfo]

          let unit = mkUnit unitInfo
          let module_ = mkModule unit moduleName
          let name = mkExternalName (mkBuiltinUnique 0) module_ (mkVarOcc symbol) noSrcSpan

          value <- liftIO $ withInterp session $ \interp -> getHValue session name >>= wormhole interp
          return $ Just $ unsafeCoerce value

lookupUnitInfo :: DynFlags -> ModuleName -> IO (Maybe UnitInfo)
lookupUnitInfo flags moduleName = do
  case exposedModulesAndPackages of
    [] -> do
      putStrLn $ "Can't find module " <> moduleNameString moduleName
      return Nothing
    (_, unitInfo) : _ ->
      return $ Just unitInfo
  where
    unitState = Session.unitState flags
    modulesAndPackages =
      lookupModuleInAllUnits unitState moduleName
    exposedModulesAndPackages =
      filter (unitIsExposed . snd) modulesAndPackages
