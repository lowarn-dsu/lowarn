import Spec.Config (configTests)
import Spec.ManualDsu (manualDsuTests)
import Spec.ProgramName (programNameTests)
import Spec.Story (storyTests)
import Spec.Transformer (transformerTests)
import Spec.UpdateId (updateIdTests)
import Spec.VersionGraph (versionGraphTests)
import Spec.VersionId (versionIdTests)
import Spec.VersionNumber (versionNumberTests)
import Test.Lowarn.Tasty (withBinarySemaphore)
import Test.Tasty (DependencyType (AllFinish), after, defaultMain, testGroup)

main :: IO ()
main =
  defaultMain
    $ testGroup
      "Lowarn"
    $ withBinarySemaphore
      ( \binarySemaphoreAction ->
          testGroup
            "Runtime"
            $ [ manualDsuTests,
                storyTests
              ]
              <*> [binarySemaphoreAction]
      )
      : ( after AllFinish "$1 == \"Runtime\""
            <$> [ versionNumberTests,
                  programNameTests,
                  versionIdTests,
                  updateIdTests,
                  transformerTests,
                  versionGraphTests,
                  configTests
                ]
        )
