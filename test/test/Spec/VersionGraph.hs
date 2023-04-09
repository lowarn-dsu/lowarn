{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Spec.VersionGraph (versionGraphTests) where

import Lowarn.Cli.VersionGraph
import Lowarn.ExampleProgram.Following.DemoInfo
import Lowarn.UpdateId
import Lowarn.VersionId
import Lowarn.VersionNumber
import Path hiding (Dir)
import System.Directory.Tree
import Test.Lowarn.DirectoryTree
import Test.Tasty
import Text.Printf

versionGraphGoldenTest :: String -> [DirTree ()] -> TestTree
versionGraphGoldenTest testName directoryChildren =
  directoryTreeGoldenTest
    testName
    (Dir "following" directoryChildren)
    $ \logFile directoryPath -> do
      validDirectoryPath <- parseAbsDir directoryPath
      versionGraph <-
        getVersionGraph
          (validDirectoryPath </> [reldir|following|])
          followingProgramName
      let mEarliestVersionNumber = earliestVersionNumber versionGraph
      writeFile logFile $
        unlines
          [ "Version graph:",
            show versionGraph,
            "Earliest version number:",
            show $ showWithDots <$> mEarliestVersionNumber,
            "Latest version number:",
            show $ showWithDots <$> latestVersionNumber versionGraph,
            "Earliest version number after earliest version number:",
            show $
              showWithDots
                <$> ( flip earliestNextVersionNumber versionGraph
                        =<< mEarliestVersionNumber
                    ),
            "Latest version number after earliest version number:",
            show $
              showWithDots
                <$> ( flip latestNextVersionNumber versionGraph
                        =<< mEarliestVersionNumber
                    )
          ]

following :: TestTree
following =
  versionGraphGoldenTest
    (show 'following)
    [ Dir
        "versions"
        [ Dir "1.0.0" [unitFile "lowarn-version-following-v1v0v0.cabal"],
          Dir "2.0.0" [unitFile "lowarn-version-following-v2v0v0.cabal"],
          Dir "3.0.0" [unitFile "lowarn-version-following-v3v0v0.cabal"]
        ],
      Dir
        "updates"
        [ Dir
            "1.0.0-2.0.0"
            [unitFile "lowarn-update-following-v1v0v0-v2v0v0.cabal"],
          Dir
            "2.0.0-3.0.0"
            [unitFile "lowarn-update-following-v2v0v0-v3v0v0.cabal"]
        ]
    ]

followingFullyConnected :: TestTree
followingFullyConnected =
  versionGraphGoldenTest
    (show 'followingFullyConnected)
    [ Dir
        "versions"
        [ Dir "1.0.0" [unitFile "lowarn-version-following-v1v0v0.cabal"],
          Dir "2.0.0" [unitFile "lowarn-version-following-v2v0v0.cabal"],
          Dir "3.0.0" [unitFile "lowarn-version-following-v3v0v0.cabal"]
        ],
      Dir
        "updates"
        [ Dir
            (printf "%s-%s" (showWithDots v1) (showWithDots v2))
            [ unitFile $
                showUpdatePackageName
                  (UpdateId followingProgramName v1 v2)
                  <> ".cabal"
            ]
          | v1 <- versionNumbers,
            v2 <- versionNumbers
        ]
    ]
  where
    versionNumbers =
      map
        versionIdVersionNumber
        [followingVersionId_1, followingVersionId_2, followingVersionId_3]

followingNoCabalFiles :: TestTree
followingNoCabalFiles =
  versionGraphGoldenTest
    (show 'followingNoCabalFiles)
    [ Dir "versions" [Dir "1.0.0" [], Dir "2.0.0" []],
      Dir
        "updates"
        [Dir "1.0.0-2.0.0" []]
    ]

followingWrongCabalFiles :: TestTree
followingWrongCabalFiles =
  versionGraphGoldenTest
    (show 'followingWrongCabalFiles)
    [ Dir
        "versions"
        [ Dir "1.0.0" [unitFile "lowarn-version-following-v2v0v0.cabal"],
          Dir "2.0.0" [unitFile "lowarn-version-following-v3v0v0.cabal"],
          Dir "3.0.0" [unitFile "lowarn-version-following-v1v0v0.cabal"]
        ],
      Dir
        "updates"
        [ Dir
            "1.0.0-2.0.0"
            [unitFile "lowarn-update-following-v2v0v0-v1v0v0.cabal"],
          Dir
            "2.0.0-3.0.0"
            [unitFile "lowarn-update-following-v3v0v0-v4v0v0.cabal"]
        ]
    ]

followingWrongProgramName :: TestTree
followingWrongProgramName =
  versionGraphGoldenTest
    (show 'followingWrongProgramName)
    [ Dir
        "versions"
        [ Dir "1.0.0" [unitFile "lowarn-version-manual-following-v1v0v0.cabal"],
          Dir "2.0.0" [unitFile "lowarn-version-manual-following-v2v0v0.cabal"],
          Dir "3.0.0" [unitFile "lowarn-version-manual-following-v3v0v0.cabal"]
        ],
      Dir
        "updates"
        [ Dir
            "1.0.0-2.0.0"
            [unitFile "lowarn-update-manual-following-v1v0v0-v2v0v0.cabal"],
          Dir
            "2.0.0-3.0.0"
            [unitFile "lowarn-update-manual-following-v2v0v0-v3v0v0.cabal"]
        ]
    ]

followingVersionZero :: TestTree
followingVersionZero =
  versionGraphGoldenTest
    (show 'followingVersionZero)
    [ Dir
        "versions"
        [ Dir "0" [unitFile "lowarn-version-following-v0.cabal"],
          Dir "1.0.0" [unitFile "lowarn-version-following-v1v0v0.cabal"],
          Dir "2.0.0" [unitFile "lowarn-version-following-v2v0v0.cabal"]
        ],
      Dir
        "updates"
        [ Dir
            "1.0.0-2.0.0"
            [unitFile "lowarn-update-following-v1v0v0-v2v0v0.cabal"]
        ]
    ]

followingUpdateZero :: TestTree
followingUpdateZero =
  versionGraphGoldenTest
    (show 'followingUpdateZero)
    [ Dir
        "versions"
        [ Dir "1.0.0" [unitFile "lowarn-version-following-v1v0v0.cabal"],
          Dir "2.0.0" [unitFile "lowarn-version-following-v2v0v0.cabal"]
        ],
      Dir
        "updates"
        [ Dir
            "0-1.0.0"
            [unitFile "lowarn-update-following-v0-v1v0v0.cabal"],
          Dir
            "1.0.0-2.0.0"
            [unitFile "lowarn-update-following-v1v0v0-v2v0v0.cabal"]
        ]
    ]

followingNoTwo :: TestTree
followingNoTwo =
  versionGraphGoldenTest
    (show 'followingNoTwo)
    [ Dir
        "versions"
        [ Dir "1.0.0" [unitFile "lowarn-version-following-v1v0v0.cabal"],
          Dir "3.0.0" [unitFile "lowarn-version-following-v3v0v0.cabal"]
        ],
      Dir
        "updates"
        [ Dir
            "1.0.0-2.0.0"
            [unitFile "lowarn-update-following-v1v0v0-v2v0v0.cabal"],
          Dir
            "2.0.0-3.0.0"
            [unitFile "lowarn-update-following-v2v0v0-v3v0v0.cabal"]
        ]
    ]

followingNoVersions :: TestTree
followingNoVersions =
  versionGraphGoldenTest
    (show 'followingNoVersions)
    [ Dir
        "updates"
        [ Dir
            "1.0.0-2.0.0"
            [unitFile "lowarn-update-following-v1v0v0-v2v0v0.cabal"]
        ]
    ]

followingNoUpdates :: TestTree
followingNoUpdates =
  versionGraphGoldenTest
    (show 'followingNoUpdates)
    [ Dir
        "versions"
        [Dir "1.0.0" [unitFile "lowarn-version-following-v1v0v0.cabal"]]
    ]

followingNothing :: TestTree
followingNothing = versionGraphGoldenTest (show 'followingNothing) []

versionGraphTests :: TestTree
versionGraphTests =
  testGroup
    "Version graph"
    [ following,
      followingFullyConnected,
      followingNoCabalFiles,
      followingWrongCabalFiles,
      followingWrongProgramName,
      followingVersionZero,
      followingUpdateZero,
      followingNoTwo,
      followingNoVersions,
      followingNoUpdates,
      followingNothing
    ]
