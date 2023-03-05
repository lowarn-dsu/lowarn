{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Data.Yaml (decodeFileEither)
import Lowarn.Cli.Config
import Lowarn.ProgramName
import Options.Applicative
import Text.Printf

newtype Options = Options
  { optionsLowarnYamlPath :: Maybe FilePath
  }

parser :: Parser Options
parser = do
  optionsLowarnYamlPath <-
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
  lowarnYamlPath <- case optionsLowarnYamlPath of
    Just p -> return p
    Nothing ->
      fail
        "Automatically finding the Lowarn configuration file is not currently supported."
  decodeFileEither lowarnYamlPath >>= \case
    Left e ->
      printf "Could not parse Lowarn CLI configuration file:\n%s" (show e)
    Right config -> do
      putStrLn $
        printf "Program name is %s." $
          unProgramName $
            lowarnConfigProgramName config
