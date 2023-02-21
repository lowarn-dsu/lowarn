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
import Data.Foldable (maximumBy)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import GHC.Core.Predicate
import GHC.Plugins hiding (TcPlugin)
import GHC.Tc.Types (tcg_mod)
import GHC.TcPlugin.API
import Lowarn.ParserCombinators (parsePackageName, readWithParser)
import Lowarn.ProgramName (showPrefixModuleName)
import Lowarn.VersionId (parseVersionPackageName, _programName)
import Text.ParserCombinators.ReadP (readP_to_S)
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
    { tcPluginInit = resolveNames,
      tcPluginSolve = solve,
      tcPluginRewrite = const emptyUFM,
      tcPluginStop = const (return ())
    }

resolveNames :: TcPluginM 'Init ResolvedNames
resolveNames = do
  currentModule <- tcg_mod . fst <$> getEnvs
  let currentUnitId = moduleUnit currentModule
      mPackageName =
        case map fst $ readP_to_S parsePackageName $ unitString currentUnitId of
          [] -> Nothing
          possiblePackageNames ->
            Just $ maximumBy (comparing length) possiblePackageNames
      mPackageProgramName =
        _programName
          <$> (readWithParser parseVersionPackageName =<< mPackageName)

  case mPackageProgramName of
    Just packageProgramName -> do
      injectModule <-
        findModule "Lowarn.Inject" (OtherPkg $ stringToUnitId "lowarn-inject")
      runtimeDataVarModule <-
        findModule
          "Lowarn.Inject.RuntimeDataVar"
          (OtherPkg $ stringToUnitId "lowarn-inject")
      localModule <-
        findModule
          (showPrefixModuleName "RuntimeDataVar" packageProgramName)
          (ThisPkg $ UnitId $ fsLit "this")
      liftM4
        ResolvedNames
        (getClass injectModule "InjectedRuntimeData")
        (getClass injectModule "InjectRuntimeData")
        (getId runtimeDataVarModule "putRuntimeDataVar")
        (getId localModule "runtimeDataVar")
    _ ->
      panic
        "Lowarn.Inject.Plugin used in non-Lowarn version package %s."
        (unitString currentUnitId)
  where
    findModule :: String -> PkgQual -> TcPluginM 'Init Module
    findModule moduleNameToFind pkgQual =
      findImportedModule
        (mkModuleName moduleNameToFind)
        pkgQual
        >>= \case
          Found _ m -> return m
          _ -> panic $ printf "Cannot find module %s." moduleNameToFind

    getClass :: Module -> String -> TcPluginM 'Init Class
    getClass classModule classString =
      tcLookupClass =<< lookupOrig classModule (mkTcOcc classString)

    getId :: Module -> String -> TcPluginM 'Init Id
    getId idModule idString =
      tcLookupId =<< lookupOrig idModule (mkVarOcc idString)

findClassConstraint :: Class -> Ct -> Maybe (Ct, Type)
findClassConstraint cls ct = do
  (cls', [t]) <- getClassPredTys_maybe (ctPred ct)
  guard (cls' == cls)
  return (ct, t)

solveInjectClassConstraint :: ResolvedNames -> (Ct, Type) -> Maybe (EvTerm, Ct)
solveInjectClassConstraint resolvedNames (ct, t) =
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

solve :: ResolvedNames -> TcPluginSolver
solve resolvedNames _ wanteds =
  return $! TcPluginOk solutions []
  where
    our_wanteds =
      mapMaybe
        (findClassConstraint $ _injectRuntimeDataClass resolvedNames)
        wanteds
    solutions =
      mapMaybe (solveInjectClassConstraint resolvedNames) our_wanteds
