{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module                  : Test.Lowarn.Property
-- SPDX-License-Identifier : MIT
-- Stability               : stable
-- Portability             : non-portable (POSIX, GHC)
--
-- Module for property testing utilities for Lowarn.
module Test.Lowarn.Property
  ( roundTripPropertyShow,
    parserCombinatorRoundTripProperty,
    jsonRoundTripProperty,
    yamlRoundTripProperty,
  )
where

import Data.Aeson
import Data.Binary.Builder
import Data.Proxy
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Encoding as Text.Lazy
import qualified Data.Yaml as Yaml
import Lowarn.ParserCombinators
import Test.QuickCheck hiding (Success)
import Text.ParserCombinators.ReadP

-- | Give the property that the result of applying a custom function to a type
-- and parsing the result is the original value, with a custom show function
-- used for error messages.
roundTripPropertyShow ::
  (Arbitrary a, Eq a) => (a -> String) -> (a -> b) -> (b -> Maybe a) -> Property
roundTripPropertyShow customShow to parse =
  propertyForAllShrinkShow arbitrary shrink (return . customShow) $
    \x -> Just x == parse (to x)

-- | Give the property that the result of parsing the result of showing a value,
-- with a given custom parser and show function, is the original value.
parserCombinatorRoundTripProperty ::
  (Arbitrary a, Eq a) => (a -> String) -> ReadP a -> Property
parserCombinatorRoundTripProperty customShow customParse =
  roundTripPropertyShow
    customShow
    customShow
    (readWithParser customParse)

-- | Give the property that the result of converting a given value to and from
-- JSON is the original value.
jsonRoundTripProperty ::
  forall a. (Arbitrary a, Eq a, ToJSON a, FromJSON a) => Proxy a -> Property
jsonRoundTripProperty =
  const $
    roundTripPropertyShow @a
      (Text.Lazy.unpack . Text.Lazy.decodeUtf8 . encode)
      (toLazyByteString . fromEncoding . toEncoding)
      decode

-- | Give the property that the result of converting a given value to and from
-- YAML is the original value.
yamlRoundTripProperty ::
  forall a. (Arbitrary a, Eq a, ToJSON a, FromJSON a) => Proxy a -> Property
yamlRoundTripProperty =
  const $
    roundTripPropertyShow @a
      (Text.unpack . Text.decodeUtf8 . Yaml.encode)
      Yaml.encode
      ( ( \case
            Left _ -> Nothing
            Right x -> Just x
        )
          . Yaml.decodeEither'
      )
