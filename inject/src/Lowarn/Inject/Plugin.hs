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

import Control.Monad
import Data.Foldable (maximumBy)
import Data.Functor
import Data.Maybe (isJust, mapMaybe)
import Data.Ord (comparing)
import GHC (Severity (SevWarning))
import GHC.Core.Predicate
import GHC.Plugins hiding (TcPlugin, getPrintUnqualified, programName, (<>))
import GHC.Tc.Types (tcg_mod)
import GHC.Tc.Types.Constraint
import GHC.Tc.Utils.Monad (getPrintUnqualified)
import GHC.TcPlugin.API
import GHC.TcPlugin.API.Internal
import GHC.Utils.Logger
import Lowarn.ParserCombinators (parsePackageName, readWithParser)
import Lowarn.ProgramName
import Lowarn.VersionId (parseVersionPackageName, _programName)
import Text.ParserCombinators.ReadP (readP_to_S)
import Text.Printf (printf)

-- | A type-checking plugin that provides instances of
-- @'Lowarn.Inject.InjectRuntimeData' a@ and
-- and @'Lowarn.Inject.InjectedRuntimeData' a@.
plugin :: Plugin
plugin =
  defaultPlugin
    { tcPlugin = Just . mkTcPlugin . injectTcPlugin,
      pluginRecompile = purePlugin
    }

data ResolvedNames = ResolvedNames
  { _isEntrypointModule :: Bool,
    _injectedRuntimeDataClass :: Class,
    _injectRuntimeDataClass :: Class,
    _putRuntimeDataVarId :: Id,
    _readRuntimeDataVarId :: Id,
    _runtimeDataVarId :: Id
  }

injectTcPlugin :: [String] -> TcPlugin
injectTcPlugin args =
  TcPlugin
    { tcPluginInit =
        resolveNamesWithArguments args >>= \case
          Left resolutionError -> do
            addWarning $
              printf
                "lowarn-inject plugin did not run due to the following error: %s"
                resolutionError
            return Nothing
          Right resolvedVariables ->
            return $ Just resolvedVariables,
      tcPluginSolve = maybe (const $ const $ return $ TcPluginOk [] []) solve,
      tcPluginRewrite = const emptyUFM,
      tcPluginStop = const $ return ()
    }

resolveNamesWithArguments ::
  [String] -> TcPluginM 'Init (Either String ResolvedNames)
resolveNamesWithArguments = \case
  [programNameString] -> do
    case readWithParser parseProgramName programNameString of
      Just programName -> resolveNames programName
      Nothing ->
        return $
          Left $
            printf
              "Argument %s is not a program name."
              programNameString
  [] ->
    return $
      Left "Program name argument not given to lowarn-inject plugin."
  _ : _ : _ ->
    return $ Left "Too many arguments given to lowarn-inject plugin."

resolveNames :: ProgramName -> TcPluginM 'Init (Either String ResolvedNames)
resolveNames programName = do
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
      isEntryPointModule =
        isJust $
          readWithParser
            (parsePrefixModuleName "EntryPoint")
            (moduleNameString $ moduleName currentModule)

  case mPackageProgramName of
    Just packageProgramName
      | packageProgramName == programName -> innerResolve isEntryPointModule
      | otherwise ->
          panic $
            printf
              "lowarn-inject plugin given program name %s, which does not match package program name %s."
              (unProgramName programName)
              (unProgramName packageProgramName)
    _
      | unitString currentUnitId == "main" ->
          innerResolve isEntryPointModule
      | otherwise ->
          panic $
            printf
              "Lowarn.Inject.Plugin used in non-Lowarn version package %s."
              (unitString currentUnitId)
  where
    findModule :: String -> PkgQual -> TcPluginM 'Init (Either String Module)
    findModule moduleNameToFind pkgQual =
      findImportedModule
        (mkModuleName moduleNameToFind)
        pkgQual
        <&> \case
          Found _ m -> Right m
          _ -> Left $ printf "Cannot find module %s." moduleNameToFind

    getClass :: String -> Module -> TcPluginM 'Init Class
    getClass classString classModule =
      tcLookupClass =<< lookupOrig classModule (mkTcOcc classString)

    getId :: String -> Module -> TcPluginM 'Init Id
    getId idString idModule =
      tcLookupId =<< lookupOrig idModule (mkVarOcc idString)

    lowarnInjectQualifier :: PkgQual
    lowarnInjectQualifier = OtherPkg $ stringToUnitId "lowarn-inject"

    mapRightM :: Monad m => (b -> m c) -> Either a b -> m (Either a c)
    mapRightM f = either (return . Left) (fmap Right . f)

    innerResolve :: Bool -> TcPluginM 'Init (Either String ResolvedNames)
    innerResolve isEntryPointModule = do
      eInjectModule <-
        findModule "Lowarn.Inject" lowarnInjectQualifier
      eRuntimeDataVarModule <-
        findModule "Lowarn.Inject.RuntimeDataVar" lowarnInjectQualifier
      eLocalModule <-
        findModule
          (showPrefixModuleName "RuntimeDataVar" programName)
          (ThisPkg $ UnitId $ fsLit "this")
      liftM5
        ( liftM5
            (ResolvedNames isEntryPointModule)
        )
        (mapRightM (getClass "InjectedRuntimeData") eInjectModule)
        (mapRightM (getClass "InjectRuntimeData") eInjectModule)
        (mapRightM (getId "putRuntimeDataVar") eRuntimeDataVarModule)
        (mapRightM (getId "readRuntimeDataVar") eRuntimeDataVarModule)
        (mapRightM (getId "runtimeDataVar") eLocalModule)

runtimeDataEvidence :: Class -> Id -> Id -> Type -> EvTerm
runtimeDataEvidence runtimeDataClass runtimeDataFunction runtimeDataVar t =
  evDataConApp
    (classDataCon runtimeDataClass)
    [t]
    [mkCoreApps (Var runtimeDataFunction) [Type t, Var runtimeDataVar]]

solveInjectClassConstraint :: ResolvedNames -> (Ct, Type) -> (EvTerm, Ct)
solveInjectClassConstraint resolvedNames (ct, t) =
  ( runtimeDataEvidence
      (_injectRuntimeDataClass resolvedNames)
      (_putRuntimeDataVarId resolvedNames)
      (_runtimeDataVarId resolvedNames)
      t,
    ct
  )

solveInjectedClassConstraint ::
  ResolvedNames -> (Ct, Type) -> TcPluginM 'Solve ((EvTerm, Ct), Ct)
solveInjectedClassConstraint resolvedNames (ct, t) = do
  hole <- newCoercionHole liftedTypeKind
  return
    ( ( case runtimeDataEvidence
          (_injectedRuntimeDataClass resolvedNames)
          (_readRuntimeDataVarId resolvedNames)
          (_runtimeDataVarId resolvedNames)
          runtimeDataType of
          EvExpr evExpr ->
            evCast evExpr $
              mkTyConAppCo
                Representational
                (classTyCon $ _injectedRuntimeDataClass resolvedNames)
                [mkHoleCo hole]
          _ ->
            panic
              "Application of InjectRuntimeData data constructor did not give an expression.",
        ct
      ),
      mkNonCanonical $
        CtWanted
          (mkPrimEqPred t runtimeDataType)
          (HoleDest hole)
          WOnly
          (ctLoc ct)
    )
  where
    runtimeDataType =
      snd $ splitAppTy $ varType $ _runtimeDataVarId resolvedNames

findClassConstraint :: Class -> Ct -> Maybe (Ct, Type)
findClassConstraint constraintClass ct = do
  (constraintClassPred, [t]) <- getClassPredTys_maybe (ctPred ct)
  guard (constraintClassPred == constraintClass)
  return (ct, t)

runtimeDataWanteds :: Class -> [Ct] -> [(Ct, Type)]
runtimeDataWanteds = mapMaybe . findClassConstraint

solve :: ResolvedNames -> TcPluginSolver
solve resolvedNames _ wanteds = do
  (injectedSolutions, newInjectedWanteds) <-
    mapAndUnzipM (solveInjectedClassConstraint resolvedNames) injectedWanteds
  return $! TcPluginOk (injectSolutions <> injectedSolutions) newInjectedWanteds
  where
    injectWanteds =
      if _isEntrypointModule resolvedNames
        then runtimeDataWanteds (_injectRuntimeDataClass resolvedNames) wanteds
        else []
    injectSolutions =
      map (solveInjectClassConstraint resolvedNames) injectWanteds
    injectedWanteds =
      runtimeDataWanteds (_injectedRuntimeDataClass resolvedNames) wanteds

addWarning :: String -> TcPluginM 'Init ()
addWarning message = do
  unsafeWithRunInTcM $ \_ -> do
    session <- getDynFlags
    logger <- getLogger
    printUnqualified <- getPrintUnqualified
    void $
      liftIO $
        putLogMsg
          logger
          session
          NoReason
          SevWarning
          noSrcSpan
          (withPprStyle (mkErrStyle printUnqualified) $ text message)
