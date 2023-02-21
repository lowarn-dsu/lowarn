{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module                  : Lowarn.Inject.Plugin
-- SPDX-License-Identifier : MIT
-- Stability               : experimental
-- Portability             : non-portable (POSIX, GHC)
--
-- Module for a plugin used to inject runtime data.
module Lowarn.Inject.Plugin (plugin) where

import Control.Monad (guard, liftM4)
import Data.Maybe (mapMaybe)
import GHC.Core.Predicate
import GHC.Data.IOEnv (getEnv, liftIO)
import GHC.Plugins
  ( ModIface_ (mi_exports, mi_globals),
    Plugin (..),
    defaultPlugin,
    getOccString,
    lookupGlobalRdrEnv,
    mkCoreApps,
    purePlugin,
  )
import GHC.Tc.Module (getModuleInterface)
import GHC.Tc.Types (Env (env_top), tcg_mod)
import GHC.TcPlugin.API
import GHC.TcPlugin.API.Internal (unsafeWithRunInTcM)
import GHC.Types.Avail (availName)
import Text.Printf (printf)

plugin :: Plugin
plugin =
  defaultPlugin
    { tcPlugin = const (Just $ mkTcPlugin injectTcPlugin),
      pluginRecompile = purePlugin
    }

data ResolvedNames = ResolvedNames
  { _injectedRuntimeDataClass :: Class,
    _injectRuntimeDataClass :: Class,
    _putRuntimeDataVarId :: Id,
    _runtimeDataVarId :: Id
  }

injectTcPlugin :: TcPlugin
injectTcPlugin =
  TcPlugin
    { tcPluginInit = getClasses,
      tcPluginSolve = solve,
      tcPluginRewrite = const emptyUFM,
      tcPluginStop = const (return ())
    }

findClassConstraint :: Class -> Ct -> Maybe (Ct, Type)
findClassConstraint cls ct = do
  (cls', [t]) <- getClassPredTys_maybe (ctPred ct)
  guard (cls' == cls)
  return (ct, t)

solveClassConstraint :: ResolvedNames -> (Ct, Type) -> Maybe (EvTerm, Ct)
solveClassConstraint resolvedNames (ct, t) =
  Just
    ( evDataConApp
        (classDataCon $ _injectRuntimeDataClass resolvedNames)
        [t]
        [ mkCoreApps
            (Var $ _putRuntimeDataVarId resolvedNames)
            [Type t, Var $ _runtimeDataVarId resolvedNames]
        ],
      ct
    )

moduleHasRuntimeDataVar :: Module -> TcPluginM 'Init Bool
moduleHasRuntimeDataVar entryPointModule = unsafeWithRunInTcM $ \_ -> do
  session <- env_top <$> getEnv
  (_, mInterface) <- liftIO $ getModuleInterface session entryPointModule
  return $ case mInterface >>= mi_globals of
    Just globals ->
      not $ null $ lookupGlobalRdrEnv globals (mkVarOcc "runtimeDataVar")
    Nothing -> False

moduleHasRuntimeDataVar' :: Module -> TcPluginM 'Init Bool
moduleHasRuntimeDataVar' entryPointModule = unsafeWithRunInTcM $ \_ -> do
  session <- env_top <$> getEnv
  (_, mInterface) <- liftIO $ getModuleInterface session entryPointModule
  return $ case mInterface of
    Just interface ->
      any
        ( \availInfo ->
            getOccString (availName availInfo) == "runtimeDataVar"
        )
        $ mi_exports interface
    Nothing -> False

getClasses :: TcPluginM 'Init (Maybe ResolvedNames)
getClasses = do
  currentModule <- tcg_mod . fst <$> getEnvs

  moduleHasRuntimeDataVar' currentModule
    >>= \case
      True -> do
        injectModule <-
          findModule "Lowarn.Inject" $ Just "lowarn-inject"
        runtimeDataVarModule <-
          findModule "Lowarn.Inject.RuntimeDataVar" $ Just "lowarn-inject"
        Just
          <$> liftM4
            ResolvedNames
            (getClass injectModule "InjectedRuntimeData")
            (getClass injectModule "InjectRuntimeData")
            (getId runtimeDataVarModule "putRuntimeDataVar")
            (getId runtimeDataVarModule "runtimeDataVar")
      False -> return Nothing
  where
    findModule :: String -> Maybe String -> TcPluginM 'Init Module
    findModule moduleNameToFind mPackageNameToFind =
      findImportedModule
        (mkModuleName moduleNameToFind)
        (maybe NoPkgQual (OtherPkg . stringToUnitId) mPackageNameToFind)
        >>= \case
          Found _ m -> return m
          _ ->
            panic $
              printf
                "Cannot find module %s in package %s."
                moduleNameToFind
                (show mPackageNameToFind)

    getClass :: Module -> String -> TcPluginM 'Init Class
    getClass classModule classString =
      tcLookupClass =<< lookupOrig classModule (mkTcOcc classString)

    getId :: Module -> String -> TcPluginM 'Init Id
    getId idModule idString =
      tcLookupId =<< lookupOrig idModule (mkVarOcc idString)

solve :: Maybe ResolvedNames -> TcPluginSolver
solve mResolvedNames _ wanteds = do
  case mResolvedNames of
    Just resolvedNames -> do
      let our_wanteds =
            mapMaybe
              (findClassConstraint $ _injectRuntimeDataClass resolvedNames)
              wanteds
      let solutions = mapMaybe (solveClassConstraint resolvedNames) our_wanteds
      return $! TcPluginOk solutions []
    Nothing -> return $! TcPluginOk [] []
