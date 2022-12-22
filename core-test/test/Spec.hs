import Spec.ManualDsu (manualDsuTests)
import Spec.ProgramName (programNameTests)
import Spec.Story (storyTests)
import Spec.TransformerId (transformerIdTests)
import Spec.VersionId (versionIdTests)
import Spec.VersionNumber (versionNumberTests)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "Lowarn"
      [ manualDsuTests,
        storyTests,
        versionNumberTests,
        programNameTests,
        versionIdTests,
        transformerIdTests
      ]
