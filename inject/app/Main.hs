{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Lowarn.Inject.Preprocessor (processFile)
import Lowarn.ProgramName (ProgramName, mkProgramName)
import Options.Applicative
import Text.Printf (printf)

data Arguments = Arguments
  { argumentsOriginalPath :: FilePath,
    argumentsInputPath :: FilePath,
    argumentsOutputPath :: FilePath,
    argumentsProgramName :: ProgramName
  }

programNameReader :: ReadM ProgramName
programNameReader = do
  programNameString <- str
  case mkProgramName programNameString of
    Just programName -> return programName
    Nothing ->
      readerError $
        printf "The given program name \"%s\" is invalid." programNameString

fileVar :: String -> Parser FilePath
fileVar = argument str . (metavar "FILE" <>) . help

parser :: Parser Arguments
parser = do
  Arguments
    <$> argumentsOriginalPath
    <*> argumentsInputPath
    <*> argumentsOutputPath
    <*> argumentsProgramName
  where
    argumentsOriginalPath = fileVar "Path to the original file."
    argumentsInputPath = fileVar "Path to the input file."
    argumentsOutputPath = fileVar "Path to the output file."
    argumentsProgramName =
      argument programNameReader $
        metavar "PROGRAMNAME" <> help "Name of the program."

parserInfo :: ParserInfo Arguments
parserInfo =
  info (helper <*> parser) $
    fullDesc
      <> progDesc "Pre-process Haskell files to inject runtime data"
      <> header "lowarn-inject - a pre-processor for injecting runtime data"

main :: IO ()
main = do
  Arguments {..} <- execParser parserInfo
  setLocaleEncoding utf8
  inputString <- readFile argumentsInputPath
  writeFile argumentsOutputPath $
    processFile argumentsOriginalPath argumentsProgramName inputString
