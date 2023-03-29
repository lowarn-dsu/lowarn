module Main (main) where

import Control.Applicative
import Control.Monad
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe
import Lowarn.ProgramName
import Lowarn.Runtime
import Lowarn.VersionId
import Lowarn.VersionNumber

main :: IO ()
main =
  runRuntime runtime False True
  where
    runtime =
      void
        $ loadVersion
          ( fromJust $
              liftA2
                VersionId
                (mkProgramName "custom-ffi")
                (mkVersionNumber $ 1 :| [0, 0])
          )
        $ Nothing
