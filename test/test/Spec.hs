import Spec.Config (configTests)
import Spec.Dsu (dsuTests)
import Spec.ProgramName (programNameTests)
import Spec.Retrofit (retrofitTests)
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
            $ [ dsuTests,
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
                  configTests,
                  retrofitTests
                ]
        )
