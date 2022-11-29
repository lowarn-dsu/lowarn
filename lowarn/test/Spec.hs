import qualified DsuTestTests
import qualified SimpleDsuTests
import Test.Tasty (TestTree, defaultMain, testGroup)

goldenTests :: TestTree
goldenTests =
  testGroup
    "Golden tests"
    [ SimpleDsuTests.goldenTests,
      DsuTestTests.goldenTests
    ]

main :: IO ()
main = defaultMain goldenTests
