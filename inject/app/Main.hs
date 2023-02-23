{-# LANGUAGE LambdaCase #-}

module Main (main) where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Lowarn.Inject.Preprocessor (processFile)
import Lowarn.ProgramName (mkProgramName)
import System.Environment (getArgs)
import Text.Printf (printf)

main :: IO ()
main = do
  setLocaleEncoding utf8
  getArgs >>= \case
    [originalPath, inputPath, outputPath, programNameString] ->
      case mkProgramName programNameString of
        Just programName ->
          readFile inputPath
            >>= writeFile outputPath
              . processFile originalPath programName
        Nothing ->
          error $ printf "Invalid program name \"%s\" given." programNameString
    _ ->
      error "Lowarn Inject preprocessor given wrong arguments."
