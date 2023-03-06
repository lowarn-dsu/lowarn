{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Exception
import Lowarn.Cli.Env
import Options.Applicative
import System.IO

newtype Options = Options
  { optionsConfigPath :: Maybe FilePath
  }

parser :: Parser Options
parser = do
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
      <> progDesc "Run programs that support DSU with Lowarn"
      <> header
        "lowarn-cli - a CLI for running programs that support DSU with Lowarn"

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
      print env
