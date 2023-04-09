module Lowarn.ExampleProgram.CustomFfi.DemoInfo
  ( customFfiProgramName,
    customFfiVersionId_1,
  )
where

import Data.Maybe
import Lowarn.ParserCombinators
import Lowarn.ProgramName
import Lowarn.VersionId
import Lowarn.VersionNumber

versionNumber1 :: VersionNumber
versionNumber1 = fromJust $ readWithParser parseWithDots "1.0.0"

customFfiProgramName :: ProgramName
customFfiProgramName = fromJust $ mkProgramName "custom-ffi"

customFfiVersionId_1 :: VersionId
customFfiVersionId_1 = VersionId customFfiProgramName versionNumber1
