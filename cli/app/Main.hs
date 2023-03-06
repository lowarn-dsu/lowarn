{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Exception
import Lowarn.Cli.Env
import Lowarn.Cli.Run
import Options.Applicative
import System.IO

data Options = Options
  { optionsCommand :: Command,
    optionsConfigPath :: Maybe FilePath
  }

data Command = RunCommand

runParser :: Parser Command
runParser = pure RunCommand

runParserInfo :: ParserInfo Command
runParserInfo =
  info (helper <*> runParser) $
    fullDesc <> progDesc "Run programs that support DSU with Lowarn"

parser :: Parser Options
parser = do
  optionsCommand <-
    hsubparser $ command "run" runParserInfo
  optionsConfigPath <-
    optional $
      strOption $
        long "lowarn-yaml"
          <> metavar "LOWARN-YAML"
          <> help "Override Lowarn CLI configuration file."

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
        RunCommand -> run env
