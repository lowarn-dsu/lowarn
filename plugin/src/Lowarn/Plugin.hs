-- |
-- Module                  : Lowarn.Plugin
-- SPDX-License-Identifier : MIT
-- Stability               : experimental
-- Portability             : non-portable (POSIX, GHC)
--
-- Module for the plugin used to augment programs to add dynamic software
-- updating.
module Lowarn.Plugin (plugin) where

import GHC.Plugins

plugin :: Plugin
plugin =
  defaultPlugin
    { installCoreToDos = install,
      pluginRecompile = purePlugin
    }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
  putMsgS "Lowarn plugin enabled."
  return todo
