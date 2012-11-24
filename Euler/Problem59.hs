{-|
  Problem 59 of Project Euler

  <http://projecteuler.net/problem=59>

-}

module Euler.Problem59 where

import Data.Char
import Data.Bits

tokenize' :: String -> [String]
tokenize' "" = []
tokenize' s =
  let firstToken = takeWhile isAlpha s
  in map toUpper firstToken : tokenize' (
    drop ((length firstToken) + 1) {- everything after the first separator -}
    s)

tokenize :: String -> [String]
tokenize s = filter (/= "") (tokenize' s)

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

{- decipher a given ciphertext with a single given key recursively, byte-by-byte -}
decipher :: String -> [Int] -> Int -> String
decipher key cipherText pos
  | pos >= (length cipherText) = ""
  | otherwise = chr ((ord (key !! (pos `mod` 3))) `xor` (cipherText !! pos)) : 
    decipher key cipherText (pos + 1) 

countEnglishWords :: String -> Int
countEnglishWords s = 
  {- 10 most common english words from Oxford English Corpus 
	 http://en.wikipedia.org/wiki/Most_common_words_in_English
  -}
  let englishWords = ["THE", "BE", "TO", "OF", "AND", "A", "IN", "THAT", "HAVE", "I"]
  in 
    length (filter (`elem` englishWords) (tokenize s))

decipherThes :: String -> [Int] -> Int
decipherThes key cipherText = 
  let plainText = decipher key cipherText 0
  in 0

{- iterate a keyspace and decipher a ciphertext with each key. record a score
   for each key of how confident we are that we found the plaintext. -}
tryCrack :: String -> [String] -> [Int] -> [Int]
tryCrack key [] cipherText = []
tryCrack key keyspace cipherText = (decipherThes key cipherText) :
  (tryCrack (head keyspace) (tail keyspace) cipherText)

crack :: (String, Int)
crack = 
  let plainText = ""
      keyspace = [[a, b, c] | a <- ['a'..'z'], b <- ['a'..'z'], c <- ['a'..'z']]
  in (plainText, strSum(plainText))
