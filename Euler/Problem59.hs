{-|
  Problem 59 of Project Euler

  <http://projecteuler.net/problem=59>

-}

module Euler.Problem59 where

import Data.Char

strSum :: String -> Int
strSum s = sum $ map ord s

crack :: (String, Int)
crack = (plainText, strSum(plainText))
  where plainText = ""
