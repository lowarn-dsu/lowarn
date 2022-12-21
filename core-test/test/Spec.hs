import Spec.ManualDsu (manualDsuTests)
import Spec.Story (storyTests)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "Lowarn tests"
      [ manualDsuTests,
        storyTests
      ]
