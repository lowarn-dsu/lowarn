{-# LANGUAGE InstanceSigs #-}

-- |
-- Module                  : Lowarn.ProgramName.Arbitrary
-- SPDX-License-Identifier : MIT
-- Stability               : stable
-- Portability             : non-portable (POSIX, GHC)
--
-- Module for 'Text.QuickCheck.Arbitrary' instances for
-- 'Lowarn.ProgramName.ProgramName'.
module Lowarn.ProgramName.Arbitrary () where

import Control.Applicative
import Data.Char
import Data.List (intercalate)
import Data.Maybe
import Lowarn.ProgramName
import Math.Combinat.Compositions (randomComposition1)
import System.Random
import Test.QuickCheck

arbitraryAsciiLower :: Gen Char
arbitraryAsciiLower = chooseEnum ('a', 'z')

arbitraryAsciiDigit :: Gen Char
arbitraryAsciiDigit = chooseEnum ('0', '9')

arbitraryAsciiLetterOrDigit :: Gen Char
arbitraryAsciiLetterOrDigit = oneof [arbitraryAsciiLower, arbitraryAsciiDigit]

arbitraryProgramNameWordWithLength :: Int -> Gen String
arbitraryProgramNameWordWithLength n = do
  firstHalfLength <- chooseInt (0, n)
  let secondHalfLength = n - firstHalfLength
  firstHalf <- vectorOf firstHalfLength arbitraryAsciiLetterOrDigit
  middle <- arbitraryAsciiLower
  secondHalf <- vectorOf secondHalfLength arbitraryAsciiLetterOrDigit
  return $ firstHalf <> (middle : secondHalf)

shrinkArbitraryProgramNameWord :: String -> [String]
shrinkArbitraryProgramNameWord programNameWord =
  [ shrunkProgramNameWord
    | shrunkProgramNameWord <- shrink programNameWord,
      all (liftA2 (||) isAsciiLower isDigit) shrunkProgramNameWord,
      any isAsciiLower shrunkProgramNameWord
  ]

instance Arbitrary ProgramName where
  arbitrary :: Gen ProgramName
  arbitrary = sized $ \n -> do
    numWords <- chooseInt (1, n + 1)
    seed <- arbitrary
    let numNonHyphensPerWord =
          fst $ randomComposition1 numWords (n + 1) $ mkStdGen seed
    programNameWords <-
      mapM arbitraryProgramNameWordWithLength numNonHyphensPerWord
    return $ fromJust $ mkProgramName $ intercalate "-" programNameWords

  shrink :: ProgramName -> [ProgramName]
  shrink programName =
    [ fromJust $ mkProgramName $ intercalate "-" shrunkProgramNameWords
      | shrunkProgramNameWords <-
          shrinkList shrinkArbitraryProgramNameWord $
            words $
              unProgramName programName,
        not $ null shrunkProgramNameWords
    ]
