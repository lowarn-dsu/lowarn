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

import Control.Applicative
import Control.Monad (guard, liftM4)
import Data.Foldable (maximumBy)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import GHC.Core.Predicate
import GHC.Plugins hiding (TcPlugin)
import GHC.Tc.Types (tcg_mod)
import GHC.TcPlugin.API
import Lowarn.ParserCombinators (parsePackageName, readWithParser)
import Lowarn.ProgramName (parseEntryPointModuleName)
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

getClasses :: TcPluginM 'Init (Maybe ResolvedNames)
getClasses = do
  currentModule <- tcg_mod . fst <$> getEnvs
  let mModuleProgramName =
        readWithParser
          parseEntryPointModuleName
          (moduleNameString $ moduleName currentModule)
      currentUnitId = unitString $ moduleUnit currentModule
      mPackageName = case map fst $ readP_to_S parsePackageName currentUnitId of
        [] -> Nothing
        possiblePackageNames ->
          Just $ maximumBy (comparing length) possiblePackageNames
      mPackageProgramName =
        _programName
          <$> (readWithParser parseVersionPackageName =<< mPackageName)

  case liftA2 (,) mModuleProgramName mPackageProgramName of
    Just (moduleProgramName, packageProgramName)
      | moduleProgramName == packageProgramName -> do
          injectModule <- findModule "Lowarn.Inject" $ Just "lowarn-inject"
          runtimeDataVarModule <-
            findModule "Lowarn.Inject.RuntimeDataVar" $ Just "lowarn-inject"
          Just
            <$> liftM4
              ResolvedNames
              (getClass injectModule "InjectedRuntimeData")
              (getClass injectModule "InjectRuntimeData")
              (getId runtimeDataVarModule "putRuntimeDataVar")
              (getId currentModule "runtimeDataVar")
    _ -> return Nothing
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
