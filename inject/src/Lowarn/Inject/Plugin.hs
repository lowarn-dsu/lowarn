-- |
-- Module                  : Lowarn.Inject.Plugin
-- SPDX-License-Identifier : MIT
-- Stability               : experimental
-- Portability             : non-portable (POSIX, GHC)
--
-- Module for a plugin used to inject runtime data.
module Lowarn.Inject.Plugin (plugin) where

import Control.Applicative
import GHC.Core.Class
import GHC.Plugins hiding (TcPlugin)
import GHC.Tc.Plugin
import GHC.Tc.Types
import GHC.Tc.Types.Constraint

plugin :: Plugin
plugin =
  defaultPlugin
    { tcPlugin = const (Just injectTcPlugin),
      pluginRecompile = purePlugin
    }

data Classes = Classes
  { _injectedRuntimeDataClass :: Class,
    _injectRuntimeDataClass :: Class
  }

injectTcPlugin :: TcPlugin
injectTcPlugin =
  TcPlugin
    { tcPluginInit = lookupClasses,
      tcPluginSolve = solve,
      tcPluginStop = const (return ())
    }

lookupClasses :: TcPluginM Classes
lookupClasses = do
  Found _ injectModule <-
    findImportedModule (mkModuleName "Lowarn.Inject") Nothing
  injectedRuntimeDataClassName <-
    lookupOrig injectModule (mkTcOcc "InjectedRuntimeData")
  injectRuntimeDataClassName <-
    lookupOrig injectModule (mkTcOcc "InjectRuntimeData")
  liftA2
    Classes
    (tcLookupClass injectedRuntimeDataClassName)
    (tcLookupClass injectRuntimeDataClassName)

solve :: Classes -> [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
solve classes _ _ wanteds =
  return $! TcPluginOk [] []
