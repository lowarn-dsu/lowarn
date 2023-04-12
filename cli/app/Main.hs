{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Exception
import Data.List.NonEmpty (NonEmpty ((:|)))
import GHC.Base (divInt)
import Lowarn.Cli.Config
import Lowarn.Cli.Env
import Lowarn.Cli.Retrofit.CommitMap
import Lowarn.Cli.Retrofit.Directory
import Lowarn.Cli.Retrofit.Patch
import Lowarn.Cli.Run
import Lowarn.Cli.VersionPath
import Lowarn.ParserCombinators
import Lowarn.VersionNumber
import Options.Applicative hiding (action)
import Options.Applicative.Help.Pretty (indent, text, vcat, (<$$>))
import Path
import Path.IO
import System.IO
import Text.Printf

data Options = Options
  { optionsConfigFilePath :: Maybe FilePath,
    optionsCommand :: Command
  }

data Command = RunCommand RunOptions | RetrofitCommand RetrofitCommand

newtype RunOptions = RunOptions {runOptionsVersion :: Maybe VersionNumber}

versionNumberReader :: ReadM VersionNumber
versionNumberReader = do
  versionNumberString <- str
  case readWithParser parseWithDots versionNumberString of
    Just versionNumber -> return versionNumber
    Nothing ->
      readerError $
        printf "The given version number \"%s\" is invalid." versionNumberString

runParser :: Parser Command
runParser = RunCommand . RunOptions <$> version
  where
    version =
      optional $
        option versionNumberReader $
          long "version"
            <> metavar "VERSION-NUMBER"
            <> help "Override the starting version to run."

runParserInfo :: ParserInfo Command
runParserInfo =
  info runParser $
    fullDesc <> progDesc "Run programs that support DSU with Lowarn"

data RetrofitCommand
  = RetrofitCleanCommand
  | RetrofitVersionCommand RetrofitVersionOptions
  | RetrofitVersionsCommand

data RetrofitVersionOptions = RetrofitVersionOptions
  { retrofitVersionOptionsVersion :: Maybe VersionNumber,
    retrofitVersionOptionsCommitIdTruncate :: Int,
    retrofitVersionOptionsCommand :: RetrofitVersionCommand
  }

retrofitCleanParser :: Parser RetrofitCommand
retrofitCleanParser = pure RetrofitCleanCommand

retrofitCleanParserInfo :: ParserInfo RetrofitCommand
retrofitCleanParserInfo =
  info retrofitCleanParser $
    fullDesc
      <> progDesc "Clear internal directories used by Lowarn CLI retrofit"

data RetrofitVersionCommand
  = RetrofitVersionApplyCommand [RetrofitVersionApplyAction]
  | RetrofitVersionSaveCommand [RetrofitVersionSaveAction]

data RetrofitVersionApplyAction
  = RetrofitVersionApplyActionSource
  | RetrofitVersionApplyActionSimplify
  | RetrofitVersionApplyActionRetrofit

retrofitVersionApplyActionReader :: ReadM [RetrofitVersionApplyAction]
retrofitVersionApplyActionReader =
  str >>= \case
    "source" -> return [RetrofitVersionApplyActionSource]
    "simplify" -> return [RetrofitVersionApplyActionSimplify]
    "retrofit" -> return [RetrofitVersionApplyActionRetrofit]
    "patches" ->
      return
        [RetrofitVersionApplyActionSimplify, RetrofitVersionApplyActionRetrofit]
    "all" ->
      return
        [ RetrofitVersionApplyActionSource,
          RetrofitVersionApplyActionSimplify,
          RetrofitVersionApplyActionRetrofit
        ]
    action -> readerError $ printf "The given action \"%s\" is invalid." action

retrofitVersionApplyParser :: Parser RetrofitVersionCommand
retrofitVersionApplyParser = RetrofitVersionApplyCommand <$> action
  where
    action =
      argument retrofitVersionApplyActionReader $
        metavar "ACTION"
          <> helpDoc
            ( Just $
                text "The action to perform."
                  <$$> text "Valid values are:"
                  <$$> indent
                    2
                    ( vcat $
                        map
                          text
                          [ "source: Generate /source from the Git repository.",
                            "simplify: Generate /simplified from /source with /simplify.patch.",
                            "retrofit: Generate /retrofitted from /simplified with /retrofit.patch.",
                            "patches: Perform simplify and then retrofit.",
                            "all: Perform source, then simplify, and then retrofit."
                          ]
                    )
            )

retrofitVersionApplyParserInfo :: ParserInfo RetrofitVersionCommand
retrofitVersionApplyParserInfo =
  info retrofitVersionApplyParser $
    fullDesc <> progDesc "Generate sources or apply patches"

data RetrofitVersionSaveAction
  = RetrofitVersionSaveActionSimplify
  | RetrofitVersionSaveActionRetrofit

retrofitVersionSaveActionReader :: ReadM [RetrofitVersionSaveAction]
retrofitVersionSaveActionReader =
  str >>= \case
    "simplify" -> return [RetrofitVersionSaveActionSimplify]
    "retrofit" -> return [RetrofitVersionSaveActionRetrofit]
    "all" ->
      return
        [RetrofitVersionSaveActionSimplify, RetrofitVersionSaveActionRetrofit]
    action -> readerError $ printf "The given action \"%s\" is invalid." action

retrofitVersionSaveParser :: Parser RetrofitVersionCommand
retrofitVersionSaveParser = RetrofitVersionSaveCommand <$> action
  where
    action =
      argument retrofitVersionSaveActionReader $
        metavar "ACTION"
          <> helpDoc
            ( Just $
                text "The action to perform."
                  <$$> text "Valid values are:"
                  <$$> indent
                    2
                    ( vcat $
                        map
                          text
                          [ "simplify: Generate /simplify.patch from /source and /simplified.",
                            "retrofit: Generate /retrofit.patch from /simplified and /retrofitted.",
                            "all: Perform simplify and retrofit."
                          ]
                    )
            )

retrofitVersionSaveParserInfo :: ParserInfo RetrofitVersionCommand
retrofitVersionSaveParserInfo =
  info retrofitVersionSaveParser $ fullDesc <> progDesc "Write patch files"

positiveIntegerReader :: ReadM Int
positiveIntegerReader = do
  integer <- auto
  if integer > 0
    then return integer
    else readerError "The given integer is not positive."

retrofitVersionParser :: Parser RetrofitCommand
retrofitVersionParser =
  RetrofitVersionCommand
    <$> (RetrofitVersionOptions <$> version <*> truncateCommit <*> subcommand)
  where
    version =
      optional $
        option versionNumberReader $
          long "version"
            <> metavar "VERSION-NUMBER"
            <> help "Override the version to manage."
    truncateCommit =
      option positiveIntegerReader $
        long "truncate"
          <> metavar "N"
          <> help
            "Truncate commit IDs to at least N characters. Values greater than 40 can break the program."
          <> value 7
          <> showDefault
    subcommand =
      hsubparser $
        command "apply" retrofitVersionApplyParserInfo
          <> command "save" retrofitVersionSaveParserInfo

retrofitVersionParserInfo :: ParserInfo RetrofitCommand
retrofitVersionParserInfo =
  info retrofitVersionParser $
    fullDesc
      <> progDesc
        "Manage individual versions of programs used by Lowarn CLI retrofit"

retrofitVersionsParser :: Parser RetrofitCommand
retrofitVersionsParser = pure RetrofitVersionsCommand

retrofitVersionsParserInfo :: ParserInfo RetrofitCommand
retrofitVersionsParserInfo =
  info retrofitVersionsParser $
    fullDesc
      <> progDesc "Manage all versions of a program used by Lowarn CLI retrofit"

retrofitParser :: Parser Command
retrofitParser = RetrofitCommand <$> retrofitCommand
  where
    retrofitCommand =
      hsubparser $
        command "clean" retrofitCleanParserInfo
          <> command "version" retrofitVersionParserInfo
          <> command "versions" retrofitVersionsParserInfo

retrofitParserInfo :: ParserInfo Command
retrofitParserInfo =
  info retrofitParser $
    fullDesc <> progDesc "Retrofit existing Haskell programs using Git"

parser :: Parser Options
parser =
  Options <$> filePath <*> subcommand
  where
    filePath =
      optional $
        strOption $
          long "lowarn-yaml"
            <> metavar "LOWARN-YAML"
            <> help "Override Lowarn CLI configuration file."
    subcommand =
      hsubparser $
        command "run" runParserInfo <> command "retrofit" retrofitParserInfo

parserInfo :: ParserInfo Options
parserInfo =
  info parser $
    fullDesc
      <> progDesc "Work with programs that support DSU with Lowarn"
      <> header
        "lowarn-cli - a CLI for working with programs that support DSU with Lowarn"

rootDirectory :: Path Abs Dir
rootDirectory = [absdir|/|]

main :: IO ()
main = do
  Options {..} <-
    customExecParser (prefs $ helpShowGlobals <> showHelpOnEmpty) parserInfo
  currentDirectory <- getCurrentDir
  configPath <- case optionsConfigFilePath of
    Just configPath -> resolveFile' configPath
    Nothing ->
      stripProperPrefix rootDirectory currentDirectory
        >>= findConfigPath rootDirectory
        >>= \case
          Just configFilePath -> return configFilePath
          Nothing -> fail "The Lowarn configuration file could not be found."
  getLowarnEnv configPath >>= \case
    Left e ->
      hPutStrLn stderr $ displayException e
    Right env@LowarnEnv {..} -> do
      case optionsCommand of
        RunCommand (RunOptions {..}) -> run env runOptionsVersion
        RetrofitCommand retrofitSubcommand ->
          case retrofitSubcommand of
            RetrofitCleanCommand -> clean lowarnEnvConfigPath
            RetrofitVersionCommand
              retrofitVersionOptions@RetrofitVersionOptions {..} ->
                case retrofitVersionOptionsCommand of
                  RetrofitVersionApplyCommand actions ->
                    mapM_
                      ( retrofitVersionApplyAction
                          env
                          retrofitVersionOptions
                          currentDirectory
                      )
                      actions
                  RetrofitVersionSaveCommand actions ->
                    mapM_
                      ( retrofitVersionSaveAction
                          env
                          retrofitVersionOptions
                          currentDirectory
                      )
                      actions
            RetrofitVersionsCommand -> fail "Not yet implemented."

defaultVersionNumber ::
  Path Abs Dir -> Path Abs Dir -> Maybe VersionNumber -> IO VersionNumber
defaultVersionNumber programPath currentDirectory =
  \case
    Just v -> return v
    Nothing ->
      case pathToVersionNumber programPath currentDirectory of
        Just v -> return v
        Nothing ->
          fail "The current directory is not a version directory."

retrofitVersionApplyAction ::
  LowarnEnv ->
  RetrofitVersionOptions ->
  Path Abs Dir ->
  RetrofitVersionApplyAction ->
  IO ()
retrofitVersionApplyAction
  env@LowarnEnv {..}
  RetrofitVersionOptions {..}
  currentDirectory
  action = do
    versionNumber <-
      defaultVersionNumber
        (parent lowarnEnvConfigPath)
        currentDirectory
        retrofitVersionOptionsVersion
    let versionNumberPath =
          versionNumberToPath (parent lowarnEnvConfigPath) versionNumber
    case action of
      RetrofitVersionApplyActionSource -> do
        versionNumberInt <- case unVersionNumber versionNumber of
          v1 :| [] -> return v1
          _ -> fail "The version number is not a single integer."
        withRetrofitDirectory
          env
          ( \retrofitDirectory -> do
              commitMap <-
                readCommitMap
                  (retrofitDirectory </> [relfile|commit-map|])
                  ((retrofitVersionOptionsCommitIdTruncate + 1) `divInt` 2)
                  >>= \case
                    Just c -> return c
                    Nothing -> fail "The commit map could not be read."
              commit <- case lookupCommitMap commitMap versionNumberInt of
                Just c -> return c
                Nothing ->
                  fail
                    "The commit could not be found in the commit map."
              copyCommitState
                retrofitDirectory
                (versionNumberPath </> [reldir|source|])
                commit
          )
      RetrofitVersionApplyActionSimplify -> applySimplifyPatch versionNumberPath
      RetrofitVersionApplyActionRetrofit -> applyRetrofitPatch versionNumberPath

retrofitVersionSaveAction ::
  LowarnEnv ->
  RetrofitVersionOptions ->
  Path Abs Dir ->
  RetrofitVersionSaveAction ->
  IO ()
retrofitVersionSaveAction
  LowarnEnv {..}
  RetrofitVersionOptions {..}
  currentDirectory
  action = do
    versionNumber <-
      defaultVersionNumber
        (parent lowarnEnvConfigPath)
        currentDirectory
        retrofitVersionOptionsVersion
    let versionNumberPath =
          versionNumberToPath (parent lowarnEnvConfigPath) versionNumber
    case action of
      RetrofitVersionSaveActionSimplify -> makeSimplifyPatch versionNumberPath
      RetrofitVersionSaveActionRetrofit -> makeRetrofitPatch versionNumberPath
