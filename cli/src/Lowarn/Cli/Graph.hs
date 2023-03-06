-- |
-- Module                  : Lowarn.Cli.Graph
-- SPDX-License-Identifier : MIT
-- Stability               : stable
-- Portability             : non-portable (GHC)
--
-- Module for Lowarn program version graphs.
module Lowarn.Cli.Graph (LowarnGraph (..), getGraph) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Lowarn.VersionNumber
import Path

newtype LowarnGraph = LowarnGraph
  { unLowarnGraph :: Map VersionNumber (Set VersionNumber)
  }

getGraph :: Path Abs Dir -> IO LowarnGraph
getGraph searchDir = return $ LowarnGraph Map.empty
