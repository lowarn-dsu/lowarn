{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
      void $
        setSessionDynFlags $
          addWay' WayDyn $
            flags {ghcMode = CompManager, ghcLink = LinkDynLib}
      liftIO . initDynLinker =<< getSession
      unLinker linker

-- | Action that gives an entity exported by a module in the package database,
-- which is linked if it hasn't already been.
load ::
  -- | The name of the module to find. @Nothing@ is given by the action if the
  -- module cannot be found.
  String ->
  -- | The name of the entity to take from the module.
  String ->
  Linker (Maybe a)
load moduleName' symbol = Linker $ do
  flags <- getSessionDynFlags
  session <- getSession

  let moduleName = mkModuleName moduleName'

  liftIO $
    lookupUnitInfo flags moduleName
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

lookupUnitInfo :: DynFlags -> ModuleName -> IO (Maybe UnitInfo)
lookupUnitInfo flags moduleName = do
  case exposedModulesAndPackages of
    [] -> do
      putStrLn $ "Can't find module " <> moduleNameString moduleName
      return Nothing
    (_, unitInfo) : _ -> return $ Just unitInfo
  where
    unitState = Session.unitState flags
    modulesAndPackages = lookupModuleInAllUnits unitState moduleName
    exposedModulesAndPackages = filter (unitIsExposed . snd) modulesAndPackages
