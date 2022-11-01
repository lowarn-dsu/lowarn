{-# LANGUAGE LambdaCase #-}

module DynamicLinker (load) where

-- import BasicTypes (SuccessFlag (..))
import DynFlags (DynFlags, Way (WayDyn), defaultFatalMessager, defaultFlushOut, ways)
-- import FastString (mkFastString)
import GHC (defaultErrorHandler, getSession, getSessionDynFlags, mkModule, mkModuleName, runGhc, setSessionDynFlags)
import GHC.Paths (libdir)
-- import GHCi (addLibrarySearchPath, initObjLinker, loadObj, lookupSymbol, resolveObjs)
import GhcMonad (liftIO)
import HscTypes (HscEnv (hsc_dynLinker))
import Linker (getHValue, initDynLinker, linkModule, linkPackages, showLinkerState)
import Module (UnitId, toInstalledUnitId)
import Name (mkExternalName)
import OccName (mkVarOcc)
import PackageConfig (PackageName (PackageName), packageConfigId)
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
      -- session <- getSession
      -- liftIO $ do
      --   initObjLinker session
      --   _ <- addLibrarySearchPath session "/home/jonathan/Code/lowarn/programs/.stack-work/dist/x86_64-linux/Cabal-3.2.1.0/build"
      --   loadObj session "/home/jonathan/Code/lowarn/programs/.stack-work/dist/x86_64-linux/Cabal-3.2.1.0/build/ValueProgram.o"
      --   success <- resolveObjs session
      --   case success of
      --     Succeeded -> putStrLn "succeeded"
      --     Failed -> putStrLn "failed"
      --   ptr <- lookupSymbol session (mkFastString symbol)
      --   print ptr
      --   return ()

      initialFlags <- getSessionDynFlags
      let waysCorrectedFlags = initialFlags {ways = [WayDyn]}
      _linkerPackages <- setSessionDynFlags waysCorrectedFlags
      flags <- getSessionDynFlags

      liftIO (lookupUnitId flags moduleName) >>= \case
        Nothing -> return Nothing
        Just unitId -> do
          session <- getSession

          liftIO $ linkPackages session [toInstalledUnitId unitId]
          liftIO $ putStrLn "linked package"

          let module_ =
                mkModule
                  unitId
                  (mkModuleName moduleName)

          liftIO $ initDynLinker session
          liftIO $ print $ ways flags

          liftIO $ linkModule session module_

          let name =
                mkExternalName
                  (mkBuiltinUnique 0)
                  module_
                  (mkVarOcc symbol)
                  noSrcSpan

          value <- liftIO $ getHValue session name

          let linker = hsc_dynLinker session
          liftIO $ showLinkerState linker flags

          return $ Just $ unsafeCoerce value

lookupUnitId :: DynFlags -> String -> IO (Maybe UnitId)
lookupUnitId flags moduleName = do
  print $ map (\(_, packageConfig) -> let (PackageName s) = PackageConfig.packageName packageConfig in s) modulesAndPackages
  case exposedModulesAndPackages of
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
