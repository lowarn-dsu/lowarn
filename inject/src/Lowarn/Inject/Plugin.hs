-- |
-- Module                  : Lowarn.Inject.Plugin
-- SPDX-License-Identifier : MIT
-- Stability               : experimental
-- Portability             : non-portable (POSIX, GHC)
--
-- Module for a plugin used to inject runtime data.
module Lowarn.Inject.Plugin (plugin) where

import GHC.Plugins

plugin :: Plugin
plugin =
  defaultPlugin
    { installCoreToDos = install,
      pluginRecompile = purePlugin
    }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
  putMsgS "Lowarn inject plugin enabled."
  return todo
