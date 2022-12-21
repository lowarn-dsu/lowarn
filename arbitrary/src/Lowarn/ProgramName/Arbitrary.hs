-- |
-- Module                  : Lowarn.ProgramName.Arbitrary
-- SPDX-License-Identifier : MIT
-- Stability               : stable
-- Portability             : portable
--
-- Module for 'Arbitrary' instances for 'ProgramName'.
module Lowarn.ProgramName.Arbitrary () where

import Control.Applicative (liftA2)
import Data.Char (isAsciiLower, isDigit)
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Lowarn.ProgramName (ProgramName, mkProgramName, unProgramName)
import Math.Combinat.Compositions (randomComposition1)
import System.Random (mkStdGen)
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
  return $ firstHalf ++ (middle : secondHalf)

shrinkArbitraryProgramNameWord :: String -> [String]
shrinkArbitraryProgramNameWord programNameWord =
  [ programNameWord'
    | programNameWord' <- shrink programNameWord,
      all (liftA2 (||) isAsciiLower isDigit) programNameWord',
      any isAsciiLower programNameWord'
  ]

instance Arbitrary ProgramName where
  arbitrary = sized $ \n -> do
    numWords <- chooseInt (1, n + 1)
    seed <- arbitrary
    let numNonHyphensPerWord =
          fst $ randomComposition1 numWords n $ mkStdGen seed
    programNameWords <-
      mapM arbitraryProgramNameWordWithLength numNonHyphensPerWord
    return $ fromJust $ mkProgramName $ intercalate "-" programNameWords

  shrink programName =
    [ fromJust $ mkProgramName $ intercalate "-" programNameWords'
      | programNameWords' <-
          shrinkList shrinkArbitraryProgramNameWord $
            words $
              unProgramName programName,
        not $ null programNameWords'
    ]
