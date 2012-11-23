{-|
  Problem 59 of Project Euler

  <http://projecteuler.net/problem=59>

-}

module Euler.Problem59 where

import Data.Char

{- add the ASCII values of each character in a string -}
strSum :: String -> Int
strSum s = sum $ map ord s

{- convert a string of comma delimited ints to a list of ints
   i.e. "1,2,33" yeilds [1,2,33] -}
splitInts :: String -> [Int]
splitInts "" = []
splitInts s =
  let firstToken = takeWhile (/= ',') s
  in read(firstToken) : splitInts (
  	drop ((length firstToken) + 1) {- everything after the first comma -}
  	s)

parseCipherText :: String -> [Int]
parseCipherText s = [0]

decipher :: String -> String -> String
decipher key cipherText = ""

crack :: (String, Int)
crack = 
  let plainText = ""
  in (plainText, strSum(plainText))
