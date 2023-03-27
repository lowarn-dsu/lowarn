{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module                  : Lowarn.Linker
-- SPDX-License-Identifier : MIT
-- Stability               : experimental
-- Portability             : non-portable (POSIX, GHC)
--
-- Module for interacting with GHC to link modules and load their entities.
module Lowarn.Linker
  ( -- * Monad
    Linker,
    runLinker,

    -- * Actions
    load,
    updatePackageDatabase,
  )
where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Data.List (minimumBy)
import Data.Maybe
import Data.Ord
import Data.Set (Set)
import qualified Data.Set as Set
import Foreign hiding (void)
import GHC hiding (load, moduleName)
import GHC.Data.FastString
import GHC.Data.ShortText
import GHC.Driver.Env
import GHC.Driver.Session
import GHC.Paths
import GHC.Unit hiding (moduleName)
import GHC.Unit.Env
import GHC.Unit.State
import GHCi.ObjLink
import System.Environment
import System.FilePath.Glob
import Text.Printf

foreign import ccall "dynamic"
  mkStablePtr :: FunPtr (IO (StablePtr a)) -> IO (StablePtr a)

data Linkable = Object FilePath | Archive FilePath | Dll FilePath
  deriving (Eq, Ord, Show)

linkable ::
  (FilePath -> a) -> (FilePath -> a) -> (FilePath -> a) -> Linkable -> a
linkable f _ _ (Object objectFile) = f objectFile
linkable _ f _ (Archive archiveFile) = f archiveFile
linkable _ _ f (Dll dllFile) = f dllFile

newtype CurrentLinkables = CurrentLinkables
  { unCurrentLinkables :: Set Linkable
  }

data LinkerData = LinkerData
  { shouldUnload :: Bool,
    isDynamic :: Bool
  }

-- | Monad for linking packages from the package database and accessing their
-- exported entities.
newtype Linker a = Linker
  { unLinker :: StateT CurrentLinkables (ReaderT LinkerData Ghc) a
  }
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Run a linker, optionally unloading code.
runLinker ::
  -- | A linker.
  Linker a ->
  -- | Whether or not to unload code (may result in segmentation faults).
  Bool ->
  -- | Whether or not to use the system linker (rather than the GHC linker).
  Bool ->
  IO a
runLinker linker shouldUnload isDynamic =
  defaultErrorHandler defaultFatalMessager defaultFlushOut $
    runGhc (Just libdir) $ do
      dynFlags <- getSessionDynFlags
      session <- getSession
      setSessionDynFlags
        =<< liftIO
          ( lookupEnv "LOWARN_PACKAGE_ENV"
              >>= \case
                Just "" -> return dynFlags
                Nothing -> return dynFlags
                Just lowarnPackageEnv ->
                  interpretPackageEnv (hsc_logger session) $
                    dynFlags {packageEnv = Just lowarnPackageEnv}
          )
      liftIO $
        initObjLinker $
          if shouldUnload then DontRetainCAFs else RetainCAFs
      runReaderT
        (evalStateT (unLinker linker) $ CurrentLinkables Set.empty)
        LinkerData {..}

-- | Action that gives an entity exported by object files corresponding to a
-- given package (and its dependencies) in the package database. The object
-- files are only linked if they haven't already been. @Nothing@ is given if the
-- module, package, or entities cannot be found.
load ::
  -- | The name of the package to load.
  String ->
  -- | The name of a symbol that exports an entity.
  String ->
  Linker (Maybe a)
load packageName entityName = Linker $ do
  session <- lift $ lift getSession

  case lookupUnitInfo session (PackageName $ fsLit packageName) of
    Nothing -> return Nothing
    Just unitInfo -> do
      previousLinkables <- unCurrentLinkables <$> get
      LinkerData {..} <- lift ask

      nextLinkables <-
        Set.fromList . catMaybes
          <$> sequence
            [ liftIO $ findLinkable isDynamic dependencyUnitInfo
              | dependencyUnitInfo <-
                  findDependencyUnitInfo session unitInfo,
                unitId dependencyUnitInfo /= rtsUnitId
            ]

      let (currentLinkables, unloadLinkables) =
            if shouldUnload
              then
                ( nextLinkables,
                  Set.difference previousLinkables nextLinkables
                )
              else (Set.union nextLinkables previousLinkables, Set.empty)

      put $ CurrentLinkables currentLinkables

      liftIO $ do
        mapM_
          (linkable loadObj loadArchive (void . loadDLL))
          (Set.difference nextLinkables previousLinkables)

        entity <-
          resolveObjs >>= \case
            False -> return Nothing
            True -> do
              lookupSymbol entityName
                >>= maybe
                  (return Nothing)
                  ( \symbolPtr ->
                      Just
                        <$> bracket
                          (mkStablePtr $ castPtrToFunPtr symbolPtr)
                          freeStablePtr
                          deRefStablePtr
                  )

        mapM_ (linkable unloadObj unloadObj unloadObj) unloadLinkables

        return entity

-- | Action that updates the package database. This uses the package environment
-- if it is specified. This can be set with the @LOWARN_PACKAGE_ENV@ environment
-- variable. If the package environment is not set, the package database is
-- instead updated by resetting the unit state and unit databases.
updatePackageDatabase :: Linker ()
updatePackageDatabase = Linker $ lift $ lift $ do
  dynFlags <- getSessionDynFlags
  session <- getSession
  case packageEnv dynFlags of
    Nothing ->
      setSession
        session
          { hsc_unit_env = (hsc_unit_env session) {ue_units = emptyUnitState},
            hsc_unit_dbs = Nothing
          }
    Just _ -> do
      setSessionDynFlags
        =<< liftIO (interpretPackageEnv (hsc_logger session) dynFlags)
  refreshedDynFlags <- getSessionDynFlags
  (unitDbs, unitState, homeUnit, platformConstants) <-
    liftIO $
      initUnits (hsc_logger session) refreshedDynFlags (hsc_unit_dbs session)
  setSessionDynFlags
    =<< liftIO (updatePlatformConstants refreshedDynFlags platformConstants)
  setSession
    session
      { hsc_unit_env =
          (hsc_unit_env session)
            { ue_units = unitState,
              ue_home_unit = homeUnit
            },
        hsc_unit_dbs = Just unitDbs
      }

lookupUnitInfo :: HscEnv -> PackageName -> Maybe UnitInfo
lookupUnitInfo session packageName = do
  unitInfo <-
    lookupUnitId unitState . indefUnit
      =<< lookupPackageName unitState packageName
  guard $ unitIsExposed unitInfo
  return unitInfo
  where
    unitState = ue_units $ hsc_unit_env session

findLinkable :: Bool -> UnitInfo -> IO (Maybe Linkable)
findLinkable isDynamic unitInfo = do
  if isDynamic
    then leastWithExtension "so" Dll
    else
      findFiles "%s/HS%s*.o" >>= \case
        [objectFile] -> return $ Just $ Object objectFile
        _ -> leastWithExtension "a" Archive
  where
    searchDirs =
      (if isDynamic then unitLibraryDynDirs else unitLibraryDirs) unitInfo
    packageName = unpackFS $ unPackageName $ unitPackageName unitInfo
    compOptions =
      CompOptions
        { characterClasses = False,
          characterRanges = False,
          numberRanges = False,
          wildcards = True,
          recursiveWildcards = False,
          pathSepInRanges = False,
          errorRecovery = True
        }

    findFiles :: String -> IO [FilePath]
    findFiles pattern =
      concat
        <$> sequence
          [ globDir1
              ( compileWith compOptions $
                  printf pattern searchDir packageName
              )
              searchDir
            | searchDirShort <- searchDirs,
              let searchDir = unpack searchDirShort
          ]

    leastWithExtension :: String -> (String -> Linkable) -> IO (Maybe Linkable)
    leastWithExtension fileExtension linkableConstructor = do
      candidateFiles <- findFiles $ "%s/libHS%s*." <> fileExtension
      return $ case candidateFiles of
        [] -> Nothing
        _ : _ ->
          Just $
            linkableConstructor $
              minimumBy (comparing length) candidateFiles

findDependencyUnitInfo :: HscEnv -> UnitInfo -> [UnitInfo]
findDependencyUnitInfo session rootUnitInfo =
  transitiveClosure [rootUnitInfo] (Set.singleton $ unitId rootUnitInfo) []
  where
    transitiveClosure :: [UnitInfo] -> Set UnitId -> [UnitInfo] -> [UnitInfo]
    transitiveClosure [] _ acc = acc
    transitiveClosure (unitInfo : unvisitedDependencies) seenDependencyIds acc =
      transitiveClosure
        (newUnvisitedDependencies <> unvisitedDependencies)
        (Set.union seenDependencyIds $ Set.fromList newUnvisitedDependencyIds)
        (unitInfo : acc)
      where
        dependencyIds = unitDepends unitInfo
        newUnvisitedDependencyIds =
          filter (`Set.notMember` seenDependencyIds) dependencyIds
        newUnvisitedDependencies =
          mapMaybe
            (lookupUnitId $ ue_units $ hsc_unit_env session)
            newUnvisitedDependencyIds
