{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

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
    liftIO,

    -- * Actions
    load,
    updatePackageDatabase,

    -- * Entity reader monad
    EntityReader,
    askEntity,
  )
where

import Control.Exception (bracket)
import Control.Monad
import Control.Monad.Free (Free, foldFree, liftF)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, put)
import Data.List (minimumBy)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import Foreign
  ( FunPtr,
    StablePtr,
    castPtrToFunPtr,
    deRefStablePtr,
    freeStablePtr,
  )
import GHC hiding (load, moduleName)
import GHC.Data.FastString
import GHC.Data.ShortText (unpack)
import GHC.Driver.Env (HscEnv (..))
import GHC.Driver.Monad
import GHC.Driver.Session
import GHC.Paths (libdir)
import GHC.Unit hiding (moduleName)
import GHC.Unit.Env (UnitEnv (..))
import GHC.Unit.State
import GHCi.ObjLink
import System.Environment (lookupEnv)
import System.FilePath.Glob (CompOptions (..), compileWith, globDir1)
import Text.Printf (printf)
import Unsafe.Coerce (unsafeCoerce)

foreign import ccall "dynamic"
  mkStablePtr :: FunPtr (IO (StablePtr a)) -> IO (StablePtr a)

data Linkable = Object FilePath | Archive FilePath
  deriving (Eq, Ord, Show)

linkable :: (FilePath -> a) -> (FilePath -> a) -> Linkable -> a
linkable f _ (Object objectFile) = f objectFile
linkable _ f (Archive archiveFile) = f archiveFile

newtype CurrentLinkables = CurrentLinkables
  { _currentLinkables :: Set Linkable
  }

newtype ShouldUnload = ShouldUnload
  { _shouldUnload :: Bool
  }

-- | Monad for linking packages from the package database and accessing their
-- exported entities.
newtype Linker a = Linker
  { unLinker :: StateT CurrentLinkables (ReaderT ShouldUnload Ghc) a
  }
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Run a linker, optionally unloading code.
runLinker ::
  -- | A linker.
  Linker a ->
  -- | Whether or not to unload code (may result in segmentation faults).
  Bool ->
  IO a
runLinker linker shouldUnload =
  defaultErrorHandler defaultFatalMessager defaultFlushOut $
    runGhc (Just libdir) $ do
      flags <- getSessionDynFlags
      session <- getSession
      flags' <-
        liftIO $
          lookupEnv "LOWARN_PACKAGE_ENV"
            >>= \case
              Just "" -> return flags
              Nothing -> return flags
              Just lowarnPackageEnv ->
                interpretPackageEnv (hsc_logger session) $
                  flags {packageEnv = Just lowarnPackageEnv}
      setSessionDynFlags flags' {ghcLink = LinkStaticLib}
      liftIO $
        initObjLinker $
          if shouldUnload then DontRetainCAFs else RetainCAFs
      runReaderT
        ( evalStateT (unLinker linker) $ CurrentLinkables Set.empty
        )
        $ ShouldUnload shouldUnload

data EntityReaderF a = EntityReaderF
  { _entityName :: String,
    _continuation :: forall b. Maybe b -> a
  }
  deriving (Functor)

-- | Monad for getting entities from object files just loaded by the linker.
type EntityReader = Free EntityReaderF

-- | Action that gives an entity with a given name (if it exists) from the
-- 'EntityReader'\'s environment.
askEntity :: String -> EntityReader (Maybe a)
askEntity entityName = liftF (EntityReaderF entityName (fmap unsafeCoerce))

-- | Action that gives entities exported by object files corresponding to a
-- given package (and its dependencies) in the package database. The object
-- files are only linked if they haven't already been. @Nothing@ is given if the
-- module, package, or entities cannot be found.
load ::
  -- | The name of the package to load.
  String ->
  -- | A computation in the 'EntityReader' monad that gives entities from the
  -- loaded object files.
  EntityReader (Maybe a) ->
  Linker (Maybe a)
load packageName' entityReader = Linker $ do
  session <- lift $ lift getSession

  case lookupUnitInfo session (PackageName $ mkFastString packageName') of
    Nothing -> return Nothing
    Just unitInfo -> do
      previousLinkables <- _currentLinkables <$> get
      shouldUnload <- _shouldUnload <$> lift ask

      nextLinkables <-
        Set.fromList . catMaybes
          <$> sequence
            [ liftIO $ findLinkable dependencyUnitInfo
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
          (linkable loadObj loadArchive)
          (Set.difference nextLinkables previousLinkables)

        output <-
          resolveObjs >>= \case
            False -> return Nothing
            True ->
              foldFree
                ( \(EntityReaderF entityName continuation) -> do
                    mx <-
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
                    return $ continuation mx
                )
                entityReader

        mapM_ (linkable unloadObj unloadObj) unloadLinkables

        return output

-- | Action that updates the package database. This uses the package environment
-- if it is specified. This can be set with the @LOWARN_PACKAGE_ENV@ environment
-- variable. If the package environment is not set, the package database is
-- instead updated by resetting the unit state and unit databases.
updatePackageDatabase :: Linker ()
updatePackageDatabase = Linker $ lift $ lift $ do
  flags <- getSessionDynFlags
  session <- getSession
  case packageEnv flags of
    Nothing ->
      setSession $
        session
          { hsc_unit_env = (hsc_unit_env session) {ue_units = emptyUnitState},
            hsc_unit_dbs = Nothing
          }
    Just _ -> do
      setSessionDynFlags
        =<< liftIO (interpretPackageEnv (hsc_logger session) flags)
  flags' <- getSessionDynFlags
  (unitDbs, unitState, homeUnit, platformConstants) <-
    liftIO $ initUnits (hsc_logger session) flags' (hsc_unit_dbs session)
  setSessionDynFlags
    =<< liftIO (updatePlatformConstants flags' platformConstants)
  setSession $
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
  unless (unitIsExposed unitInfo) Nothing
  return unitInfo
  where
    unitState = ue_units $ hsc_unit_env session

findLinkable :: UnitInfo -> IO (Maybe Linkable)
findLinkable unitInfo = do
  findFiles "%s/HS%s*.o" >>= \case
    [objectFile] -> return $ Just $ Object objectFile
    _ -> do
      archiveFiles <- findFiles "%s/libHS%s*.a"
      return $ case archiveFiles of
        [] -> Nothing
        _ : _ -> Just $ Archive $ minimumBy (comparing length) archiveFiles
  where
    searchDirs = unitLibraryDirs unitInfo
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

findDependencyUnitInfo :: HscEnv -> UnitInfo -> [UnitInfo]
findDependencyUnitInfo session rootUnitInfo =
  transitiveClosure [rootUnitInfo] (Set.singleton $ unitId rootUnitInfo) []
  where
    transitiveClosure :: [UnitInfo] -> Set UnitId -> [UnitInfo] -> [UnitInfo]
    transitiveClosure [] _ acc = acc
    transitiveClosure (unitInfo : unvisitedDependencies) seenDependencyIds acc =
      transitiveClosure
        (newUnvisitedDependencies ++ unvisitedDependencies)
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
