{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.Framework.TH (defaultMainGenerator)

import Test.HUnit
import Test.Framework.Providers.HUnit (testCase)

import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2 (testProperty)

{- import qualified Data.ByteString.Lazy.Char8 as L -}

import Euler.Problem59 (crack, strSum, splitInts)

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
