{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Exception
import Lowarn.Cli.Env
import Lowarn.Cli.Run
import Options.Applicative
import System.IO
import Lowarn.VersionNumber
import Lowarn.ParserCombinators
import Text.Printf

data Options = Options
  { optionsCommand :: Command,
    optionsConfigPath :: Maybe FilePath
  }

newtype Command = RunCommand RunOptions

newtype RunOptions = RunOptions
  {
    runOptionsVersion :: Maybe VersionNumber
  }

versionNumberReader :: ReadM VersionNumber
versionNumberReader = do
  versionNumberString <- str
  case readWithParser parseWithDots versionNumberString of
    Just versionNumber -> return versionNumber
    Nothing ->
      readerError $
        printf "The given version number \"%s\" is invalid." versionNumberString

runParser :: Parser Command
runParser = do
  runOptionsVersion <-
    optional $
      option versionNumberReader $
        long "version"
          <> metavar "VERSION-NUMBER"
          <> help "Override the starting version to run."
  return $ RunCommand $ RunOptions {..}

runParserInfo :: ParserInfo Command
runParserInfo =
  info (helper <*> runParser) $
    fullDesc <> progDesc "Run programs that support DSU with Lowarn"

parser :: Parser Options
parser = do
  optionsConfigPath <-
    optional $
      strOption $
        long "lowarn-yaml"
          <> metavar "LOWARN-YAML"
          <> help "Override Lowarn CLI configuration file."
  optionsCommand <-
    hsubparser $ command "run" runParserInfo
  return Options {..}

parserInfo :: ParserInfo Options
parserInfo =
  info (helper <*> parser) $
    fullDesc
      <> progDesc "Work with programs that support DSU with Lowarn"
      <> header
        "lowarn-cli - a CLI for working with programs that support DSU with Lowarn"

main :: IO ()
main = do
  Options {..} <- execParser parserInfo
  configPath <- case optionsConfigPath of
    Just p -> return p
    Nothing ->
      fail
        "Automatically finding the Lowarn configuration file is not currently supported."
  getLowarnEnv configPath >>= \case
    Left e ->
      hPutStrLn stderr $ displayException e
    Right env -> do
      case optionsCommand of
        RunCommand (RunOptions {..}) -> run env runOptionsVersion
