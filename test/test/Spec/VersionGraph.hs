{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Spec.VersionGraph (versionGraphTests) where

import Lowarn.ExampleProgram.Following.DemoInfo
import Lowarn.UpdateId
import Lowarn.VersionId
import Lowarn.VersionNumber
import Path
import System.Directory.Tree
import Test.Lowarn.DirectoryTree
import Test.Lowarn.VersionGraph
import Test.Tasty
import Text.Printf

followingVersionGraphGoldenTest ::
  String -> Path Rel Dir -> [DirTree ()] -> TestTree
followingVersionGraphGoldenTest testName =
  versionGraphGoldenTest testName followingProgramName

following :: TestTree
following =
  followingVersionGraphGoldenTest
    (show 'following)
    [reldir|.|]
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
  followingVersionGraphGoldenTest
    (show 'followingFullyConnected)
    [reldir|.|]
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
  followingVersionGraphGoldenTest
    (show 'followingNoCabalFiles)
    [reldir|.|]
    [ Dir "versions" [Dir "1.0.0" [], Dir "2.0.0" []],
      Dir
        "updates"
        [Dir "1.0.0-2.0.0" []]
    ]

followingWrongCabalFiles :: TestTree
followingWrongCabalFiles =
  followingVersionGraphGoldenTest
    (show 'followingWrongCabalFiles)
    [reldir|.|]
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
  followingVersionGraphGoldenTest
    (show 'followingWrongProgramName)
    [reldir|.|]
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
  followingVersionGraphGoldenTest
    (show 'followingVersionZero)
    [reldir|.|]
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
  followingVersionGraphGoldenTest
    (show 'followingUpdateZero)
    [reldir|.|]
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
  followingVersionGraphGoldenTest
    (show 'followingNoTwo)
    [reldir|.|]
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
  followingVersionGraphGoldenTest
    (show 'followingNoVersions)
    [reldir|.|]
    [ Dir
        "updates"
        [ Dir
            "1.0.0-2.0.0"
            [unitFile "lowarn-update-following-v1v0v0-v2v0v0.cabal"]
        ]
    ]

followingNoUpdates :: TestTree
followingNoUpdates =
  followingVersionGraphGoldenTest
    (show 'followingNoUpdates)
    [reldir|.|]
    [ Dir
        "versions"
        [Dir "1.0.0" [unitFile "lowarn-version-following-v1v0v0.cabal"]]
    ]

followingNothing :: TestTree
followingNothing =
  followingVersionGraphGoldenTest (show 'followingNothing) [reldir|.|] []

dirTreesWithRetrofitted :: [DirTree ()]
dirTreesWithRetrofitted =
  [ Dir
      "versions"
      [ Dir
          "1.0.0"
          [ Dir "retrofitted" [unitFile "lowarn-version-following-v1v0v0.cabal"]
          ],
        Dir
          "2.0.0"
          [ Dir "retrofitted" [unitFile "lowarn-version-following-v2v0v0.cabal"]
          ]
      ],
    Dir
      "updates"
      [ Dir
          "1.0.0-2.0.0"
          [unitFile "lowarn-update-following-v1v0v0-v2v0v0.cabal"]
      ]
  ]

followingCabalDirectory :: TestTree
followingCabalDirectory =
  followingVersionGraphGoldenTest
    (show 'followingCabalDirectory)
    [reldir|retrofitted|]
    dirTreesWithRetrofitted

followingCabalDirectoryWithoutArgument :: TestTree
followingCabalDirectoryWithoutArgument =
  followingVersionGraphGoldenTest
    (show 'followingCabalDirectoryWithoutArgument)
    [reldir|.|]
    dirTreesWithRetrofitted

followingCabalDirectoryWithoutDirectories :: TestTree
followingCabalDirectoryWithoutDirectories =
  followingVersionGraphGoldenTest
    (show 'followingCabalDirectoryWithoutDirectories)
    [reldir|retrofitted|]
    [ Dir
        "versions"
        [ Dir "1.0.0" [unitFile "lowarn-version-following-v1v0v0.cabal"],
          Dir "2.0.0" [unitFile "lowarn-version-following-v2v0v0.cabal"]
        ],
      Dir
        "updates"
        [ Dir
            "1.0.0-2.0.0"
            [unitFile "lowarn-update-following-v1v0v0-v2v0v0.cabal"]
        ]
    ]

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
      followingNothing,
      followingCabalDirectory,
      followingCabalDirectoryWithoutArgument,
      followingCabalDirectoryWithoutDirectories
    ]
