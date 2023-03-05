{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module                  : Lowarn.TH
-- SPDX-License-Identifier : MIT
-- Stability               : stable
-- Portability             : portable
--
-- Module for using Template Haskell to generate code for Lowarn.
module Lowarn.TH (entryPointExportDeclarations, updateExportDeclarations) where

import Control.Applicative
import Control.Monad
import Foreign hiding (void)
import Language.Haskell.TH
import Lowarn.ParserCombinators
import Lowarn.UpdateId
import Lowarn.VersionId
import Lowarn.VersionNumber
import Text.ParserCombinators.ReadP
import Text.Printf

exportDeclarations ::
  String -> ReadP a -> String -> (a -> String) -> Name -> Q [Dec]
exportDeclarations exportNameString packageNameParser idType getExport name = do
  exportType <- [t|IO (StablePtr ($(reifyType name)))|]
  exportName <- newName exportNameString
  packageName <-
    maybe
      (fail "Could not find package name corresponding to name.")
      return
      $ namePackage name
  if packageName == "main"
    then do
      reportWarning "Package name was \"main\", so export was not generated. This may happen when using HLS."
      return []
    else do
      exportId <-
        maybe
          ( fail $
              printf
                "Could not find %s ID corresponding to package name %s."
                idType
                packageName
          )
          return
          $ readWithParser
            (parsePackageNameAndExtension packageNameParser)
            packageName
      return
        [ SigD exportName exportType,
          ValD
            (VarP exportName)
            (NormalB $ AppE (VarE 'newStablePtr) (VarE name))
            [],
          ForeignD $ ExportF CCall (getExport exportId) exportName exportType
        ]

parsePackageNameAndExtension :: ReadP a -> ReadP a
parsePackageNameAndExtension packageNameParser = do
  packageName <- packageNameParser
  void $ char '-'
  void parseWithDots
  void $ char '-'
  void $ manyTill (satisfy $ (/=) '-') eof
  return packageName

-- | Generate code that exports @hs_entryPoint_v1v2v3@ of type
-- @'IO' ('StablePtr' ('Lowarn.EntryPoint' a))@ with the C foreign function
-- interface, given the name of a value of type @'Lowarn.Entrypoint' a@.
-- @v1v2v3@ represents the version number of the program version corresponding
-- to the package that the given name is found in.
entryPointExportDeclarations :: Name -> Q [Dec]
entryPointExportDeclarations =
  exportDeclarations
    "hsEntryPoint"
    parseVersionPackageName
    "version"
    $ showEntryPointExport . versionIdVersionNumber

-- | Generate code that exports @hs_update_v1v2v3_v1v2v4@ of type
-- @'IO' ('StablePtr' ('Lowarn.Update' a b))@ with the C foreign function
-- interface, given the name of a value of type @'Lowarn.Update' a b@. @v1v2v3@
-- and @v1v2v4@ represent the previous and next version numbers of the update
-- corresponding to the package that the given name is found in.
updateExportDeclarations :: Name -> Q [Dec]
updateExportDeclarations =
  exportDeclarations
    "hsUpdate"
    parseUpdatePackageName
    "update"
    $ liftA2
      showUpdateExport
      updateIdPreviousVersionNumber
      updateIdNextVersionNumber
