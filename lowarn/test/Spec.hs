import qualified SimpleDsuTests
import Test.Tasty (TestTree, defaultMain, testGroup)

goldenTests :: TestTree
goldenTests =
  testGroup
    "Golden tests"
    [SimpleDsuTests.goldenTests]

main :: IO ()
main = defaultMain goldenTests
