{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Spec.Retrofit (retrofitTests) where

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Lowarn.Cli.Config
import Lowarn.Cli.Env
import Lowarn.Cli.Retrofit.BranchName
import Lowarn.Cli.Retrofit.CommitMap
import Lowarn.Cli.Retrofit.Directory
import Lowarn.Cli.Retrofit.Patch
import Lowarn.ExampleProgram.Following.DemoInfo
import Path
import Path.IO
import System.Directory.Tree
import System.IO
import System.Process
import Test.Lowarn.DirectoryTree
import Test.Tasty
import Text.Printf
import URI.ByteString

retrofit :: TestTree
retrofit =
  directoryTreeGoldenTest
    (show 'retrofit)
    (Dir "following" [])
    $ \logFile directoryPath -> do
      logHandle <- openFile logFile WriteMode
      let failPrint s = hPutStrLn logHandle s >> fail s
      validDirectoryPath <-
        maybe (failPrint "Could not parse directory path") return $
          parseAbsDir directoryPath
      let followingDirectoryPath = validDirectoryPath </> [reldir|following|]
      let printFileIfExists path =
            doesFileExist path >>= \case
              True -> do
                hPutStrLn logHandle . printf "File %s:" . toFilePath
                  =<< stripProperPrefix validDirectoryPath path
                readFile (toFilePath path) >>= hPutStrLn logHandle
              False ->
                failPrint $ printf "File %s does not exist." $ toFilePath path

      let printDirectoryTree = do
            (_, rgOut, _) <-
              readCreateProcessWithExitCode
                ( ( proc
                      "rg"
                      [ "--no-require-git",
                        "--files",
                        "--hidden",
                        "--iglob",
                        "!.git/",
                        "--no-ignore-parent",
                        "--sort",
                        "path"
                      ]
                  )
                    { env = Just [],
                      cwd = Just $ toFilePath validDirectoryPath
                    }
                )
                ""
            hPutStr logHandle rgOut

      uri <- case parseURI strictURIParserOptions $
        Text.encodeUtf8 $
          Text.pack "https://github.com/lowarn-dsu/test-repo.git" of
        Left e -> failPrint $ printf "URI parse error: %s" $ show e
        Right u -> return u
      branchName <-
        maybe (failPrint "Invalid branch name") return $ mkBranchName "main"

      let retrofitConfig = LowarnRetrofitConfig uri branchName
          config =
            LowarnConfig
              followingProgramName
              False
              True
              [reldir|retrofitted/|]
              (Just retrofitConfig)
          lowarnEnv =
            LowarnEnv config (followingDirectoryPath </> [relfile|lowarn.yaml|])

      withRetrofitDirectory lowarnEnv $ \retrofitDirectory -> do
        hPutStrLn logHandle "Initial directory tree:"
        printDirectoryTree

        commitMap <-
          maybe (failPrint "Could not read commit map.") return
            =<< readCommitMap (retrofitDirectory </> [relfile|commit-map|]) 20
        commitId <-
          maybe (failPrint "Could not lookup commit map.") return $
            lookupCommitMap commitMap 2
        hPutStrLn logHandle $ printf "Commit ID: %s" commitId

        let versionPath = followingDirectoryPath </> [reldir|versions/1000|]

        copyCommitState
          retrofitDirectory
          (versionPath </> [reldir|source|])
          commitId
        hPutStrLn logHandle "Directory tree after copying commit state:"
        printDirectoryTree

        copyDirRecur
          (versionPath </> [reldir|source|])
          (versionPath </> [reldir|simplified|])
        writeFile
          (toFilePath $ versionPath </> [relfile|simplified/text.txt|])
          "Hello, world!"
        createDirIfMissing False $
          versionPath </> [reldir|simplified/ignore-directory|]
        writeFile
          ( toFilePath $
              versionPath </> [relfile|simplified/ignore-directory/ignored.txt|]
          )
          "Ignored."
        removeFile $ versionPath </> [relfile|simplified/README.md|]
        copyDirRecur
          (versionPath </> [reldir|simplified|])
          (versionPath </> [reldir|retrofitted|])
        writeFile
          (toFilePath $ versionPath </> [relfile|retrofitted/text.txt|])
          "Hello, Lowarn!"

        makeSimplifyPatch versionPath
        makeRetrofitPatch versionPath
        hPutStrLn logHandle "Directory tree after applying patches:"
        printDirectoryTree
        printFileIfExists $ versionPath </> [relfile|simplify.patch|]
        printFileIfExists $ versionPath </> [relfile|retrofit.patch|]

        removeDirRecur $ versionPath </> [reldir|simplified|]
        hPutStrLn logHandle "Directory tree after removing simplified:"
        printDirectoryTree

        applySimplifyPatch versionPath
        hPutStrLn logHandle "Directory tree after applying simplify patch:"
        printDirectoryTree
        printFileIfExists $ versionPath </> [relfile|simplified/text.txt|]

        writeFile
          (toFilePath $ versionPath </> [relfile|simplified/text.txt|])
          "Hello... world!"
        applyRetrofitPatch versionPath
        hPutStrLn logHandle "Directory tree after applying retrofit patch:"
        printDirectoryTree
        printFileIfExists $ versionPath </> [relfile|retrofitted/text.txt|]

      clean followingDirectoryPath
      hPutStrLn logHandle "Directory tree after clean:"
      printDirectoryTree
      hClose logHandle

retrofitTests :: TestTree
retrofitTests =
  testGroup
    "Retrofit"
    [ retrofit
    ]
