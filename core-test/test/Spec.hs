import Spec.ManualDsuTest (manualDsuTests)
import Spec.StoryTest (storyTests)
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "Lowarn tests"
      [ manualDsuTests,
        storyTests
      ]
