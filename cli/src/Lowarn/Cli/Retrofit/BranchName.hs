{-# LANGUAGE InstanceSigs #-}

-- |
-- Module                  : Lowarn.Cli.Retrofit.BranchName
-- SPDX-License-Identifier : MIT
-- Stability               : stable
-- Portability             : non-portable (POSIX, GHC)
--
-- Module for program names.
module Lowarn.Cli.Retrofit.BranchName
  ( BranchName,
    mkBranchName,
    unBranchName,
  )
where

import Data.Char
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Data.List.Split
import qualified Data.Text as Text
import Data.Yaml
import Text.Printf

-- | A branch name used by Git, validated according to the rules found in
-- <https://git-scm.com/docs/git-check-ref-format>.
newtype BranchName = BranchName
  { -- | Convert a branch name to a 'String'.
    unBranchName :: String
  }
  deriving (Eq, Show)

-- | Create a 'BranchName' from a 'String'.
--
-- ==== __Examples__
--
-- >>> mkBranchName "foo"
-- Just (BranchName {unBranchName = "foo"})
--
-- >>> mkBranchName "foo/bar"
-- Just (BranchName {unBranchName = "foo/bar"})
--
-- >>> mkBranchName "foo/"
-- Nothing
--
-- >>> mkBranchName "/foo"
-- Nothing
--
-- >>> mkBranchName "foo//bar"
-- Nothing
--
-- >>> mkBranchName "foo..bar"
-- Nothing
--
-- >>> mkBranchName "foo/bar.lock/baz"
-- Nothing
--
-- >>> mkBranchName "foo/.bar/baz"
-- Nothing
--
-- >>> mkBranchName "foo/-bar/baz"
-- Nothing
--
-- >>> mkBranchName "foo-bar"
-- Just (BranchName {unBranchName = "foo-bar"})
--
-- >>> mkBranchName "foo bar"
-- Nothing
--
-- >>> mkBranchName "foo~bar"
-- Nothing
--
-- >>> mkBranchName "foo^bar"
-- Nothing
--
-- >>> mkBranchName "foo:bar"
-- Nothing
--
-- >>> mkBranchName "foo?bar"
-- Nothing
--
-- >>> mkBranchName "foo*bar"
-- Nothing
--
-- >>> mkBranchName "foo[bar"
-- Nothing
--
-- >>> mkBranchName "foo\\bar"
-- Nothing
--
-- >>> mkBranchName "foo."
-- Nothing
--
-- >>> mkBranchName "foo.bar"
-- Just (BranchName {unBranchName = "foo.bar"})
--
-- >>> mkBranchName "foo@{bar}"
-- Nothing
--
-- >>> mkBranchName "@"
-- Nothing
--
-- >>> mkBranchName ""
-- Nothing
--
-- >>> mkBranchName "🙂"
-- Just (BranchName {unBranchName = "\128578"})
mkBranchName :: String -> Maybe BranchName
mkBranchName s =
  if and $ conditions <*> [s] then Just $ BranchName s else Nothing
  where
    conditions :: [String -> Bool]
    conditions =
      [ -- Non-empty rule
        not . null,
        -- Rule 1, 6
        and . (slashSeparatedComponentConditions <*>) . splitOn "/",
        -- Rule 3
        not . (".." `isInfixOf`),
        -- Rule 4, 5, 10
        and . (characterConditions <*>),
        -- Rule 7
        not . ("." `isSuffixOf`),
        -- Rule 8
        not . ("@{" `isInfixOf`),
        -- Rule 9
        (/=) "@"
      ]

    slashSeparatedComponentConditions :: [String -> Bool]
    slashSeparatedComponentConditions =
      [ -- Rule 6
        not . null,
        -- Rule 1
        not . (".lock" `isSuffixOf`),
        -- Rule 1
        not . ("." `isPrefixOf`),
        -- Branch rule
        not . ("-" `isPrefixOf`)
      ]

    characterConditions :: [Char -> Bool]
    characterConditions =
      [ -- Rule 4
        not . isControl,
        -- Rule 4, 5, 10
        not . (`elem` " ~^:?*[\\")
      ]

instance ToJSON BranchName where
  toJSON :: BranchName -> Value
  toJSON = toJSON . unBranchName

instance FromJSON BranchName where
  parseJSON :: Value -> Parser BranchName
  parseJSON = withText "BranchName" $ \branchNameText ->
    case mkBranchName $ Text.unpack branchNameText of
      Just programName -> return programName
      Nothing ->
        fail $ printf "Could not parse branch name %s." branchNameText
