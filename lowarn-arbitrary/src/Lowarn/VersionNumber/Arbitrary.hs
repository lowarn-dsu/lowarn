module Lowarn.VersionNumber.Arbitrary () where

import Data.List.NonEmpty (fromList, toList)
import Data.Maybe (fromJust)
import Lowarn.VersionNumber (VersionNumber, mkVersionNumber, unVersionNumber)
import Test.QuickCheck

instance Arbitrary VersionNumber where
  arbitrary = sized $ \n -> do
    numVersionNumberComponents <- chooseInt (1, log2 n + 1)
    fromJust . mkVersionNumber . fromList
      <$> vectorOf numVersionNumberComponents arbitrarySizedNatural
    where
      log2 :: Int -> Int
      log2 n = if n <= 1 then 0 else 1 + log2 (n `div` 2)

  shrink versionNumber =
    [ fromJust $ mkVersionNumber versionNumberComponents'
      | versionNumberComponents' <-
          fromList . getNonEmpty
            <$> shrink (NonEmpty $ toList $ unVersionNumber versionNumber),
        all (>= 0) versionNumberComponents'
    ]
