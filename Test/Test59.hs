{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.Framework.TH (defaultMainGenerator)

import Test.HUnit
import Test.Framework.Providers.HUnit (testCase)

import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Euler.Problem59 (crack, strSum)

main = $(defaultMainGenerator)

case_strSum_1_char = 
  strSum "a" @?= 97

case_strSum_2_char = 
  strSum "ab" @?= 97 + 98
