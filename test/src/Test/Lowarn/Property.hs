{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module                  : Test.Lowarn.Property
-- SPDX-License-Identifier : MIT
-- Stability               : stable
-- Portability             : portable
--
-- Module for property testing utilities for Lowarn.
module Test.Lowarn.Property
  ( roundTripPropertyShow,
    parserCombinatorRoundTripProperty,
    aesonRoundTripProperty,
  )
where

import Data.Aeson
import Data.Proxy
import Data.Text.Lazy
import Data.Text.Lazy.Encoding
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
  roundTripPropertyShow customShow customShow (readWithParser customParse)

-- | Give the property that the result of converting a given value to and from
-- JSON is the original value.
aesonRoundTripProperty ::
  forall a. (Arbitrary a, Eq a, ToJSON a, FromJSON a) => Proxy a -> Property
aesonRoundTripProperty =
  const $
    roundTripPropertyShow @a
      (unpack . decodeUtf8 . encode)
      toJSON
      ( ( \case
            Error _ -> Nothing
            Success x -> Just x
        )
          . fromJSON
      )
