{-# LANGUAGE ApplicativeDo #-}

module Main (main) where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Lowarn.Inject.Preprocessor (processFile)
import Lowarn.ProgramName (ProgramName, mkProgramName)
import Options.Applicative
import Text.Printf (printf)

data Arguments = Arguments
  { _originalPath :: FilePath,
    _inputPath :: FilePath,
    _outputPath :: FilePath,
    _programName :: ProgramName
  }

programNameReader :: ReadM ProgramName
programNameReader = do
  programNameString <- str
  case mkProgramName programNameString of
    Just programName -> return programName
    Nothing ->
      readerError $
        printf
          "The given program name \"%s\" is invalid."
          programNameString

parser :: Parser Arguments
parser = do
  originalPath <-
    argument str $ metavar "FILE" <> help "Path to the original file."
  inputPath <- argument str $ metavar "FILE" <> help "Path to the input file."
  outputPath <- argument str $ metavar "FILE" <> help "Path to the output file."
  programName <-
    argument programNameReader $
      metavar "PROGRAMNAME" <> help "Name of the program."

  return $ Arguments originalPath inputPath outputPath programName

parserInfo :: ParserInfo Arguments
parserInfo =
  info (helper <*> parser) $
    fullDesc
      <> progDesc "Pre-process Haskell files to inject runtime data"
      <> header "lowarn-inject - a pre-processor for injecting runtime data"

main :: IO ()
main = do
  arguments <- execParser parserInfo
  setLocaleEncoding utf8
  inputString <- readFile $ _inputPath arguments
  writeFile (_outputPath arguments) $
    processFile (_originalPath arguments) (_programName arguments) inputString
