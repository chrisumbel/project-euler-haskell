{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.Framework.TH (defaultMainGenerator)

import Test.HUnit
import Test.Framework.Providers.HUnit (testCase)

import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2 (testProperty)

{- import qualified Data.ByteString.Lazy.Char8 as L -}

import Euler.Problem59 (crack, strSum, splitInts, tryCrack, decipher, countSubStrings)

main = $(defaultMainGenerator)

{- 
readCipherText :: String
readCipherText = L.readFile "./data/cipher1.txt" 
-}

{- strSum -}
case_strSum_1_char = 
  strSum "a" @?= 97

case_strSum_2_char = 
  strSum "ab" @?= 97 + 98

{- splitInts -}
case_splitInts_single = 
  (splitInts "1") @?= [1]

case_splitInts_singles = 
  (splitInts "1, 2") @?= [1, 2]

case_splitInts_multiple = 
  (splitInts "1, 23") @?= [1, 23]

{- decipher -}
case_decipher_matching_lengths =
  decipher "aaa" [97, 97, 97] 0 @?= "\NUL\NUL\NUL"

case_decipher_longer =
  decipher "aaa" [97, 97, 97, 97] 0 @?= "\NUL\NUL\NUL\NUL"

case_decipher_shorter =
  decipher "aaa" [97, 97] 0 @?= "\NUL\NUL"

case_decipher_xor =
  decipher "abc" [0, 0, 0, 97, 98, 99, 2, 3, 4] 0 @?= "abc\NUL\NUL\NULcag"

{- countSubStrings -}
case_countSubStrings_ = 
  countSubStrings "the " "the thing is the other thing of the " @?= 3

{-case_tryCrack_
  tryCrack "aaa" [[a, b, c] | a <- ['a'..'z'], b <- ['a'..'z'], c <- ['a'..'z']] []
-}
