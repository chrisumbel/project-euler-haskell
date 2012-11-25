{-|
  Problem 59 of Project Euler

  <http://projecteuler.net/problem=59>

  Crack a given ciphertext with no konwn plaintext using a keyspace of 3 
  lower-case chars 26 ** 3 = 17576
-}

module Euler.Problem59 where

import Data.Char
import Data.Bits

type KeySpace = [String]
type CipherText = [Int]

{- tokenize a string seperated by non-alpha characters -}
tokenize' :: String -> [String]
tokenize' "" = []
tokenize' s =
  let firstToken = takeWhile isAlpha s
  in map toUpper firstToken : tokenize' (
    drop ((length firstToken) + 1) {- everything after the first separator -}
    s)

{- tokenize a string seperated by non-alpha characters. remove empty strings.
   public interface -}
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

parseCipherText :: String -> CipherText
parseCipherText s = [0]

{- decipher a given ciphertext with a single given key recursively, byte-by-byte -}
decipher :: String -> CipherText -> Int -> String
decipher key cipherText pos
  | pos >= (length cipherText) = ""
  | otherwise = chr ((ord (key !! (pos `mod` 3))) `xor` (cipherText !! pos)) : 
    decipher key cipherText (pos + 1) 

{- tokenize a given text and count the number of occurrences of the 10
   most commonly used English words. -}
countEnglishWords :: String -> Int
countEnglishWords s = 
  {- 25 most common english words from Oxford English Corpus 
	 http://en.wikipedia.org/wiki/Most_common_words_in_English
  -}
  let englishWords = ["THE", "BE", "TO"] {- , "OF", "AND", "A", "IN", "THAT", "HAVE", 
                      "I", "IT", "FOR", "NOT", "ON", "WITH", "HE", "AS", "YOU", 
                      "DO", "AT", "THIS", "BUT", "HIS", "BY", "FROM"] -}
  in 
    length (filter (`elem` englishWords) (tokenize s))

{- decipher a text with a given key and score it -}
decipherAndScore :: String -> CipherText -> Int
decipherAndScore key cipherText = 
  countEnglishWords (decipher key cipherText 0)

{- iterate a keyspace and decipher a ciphertext with each key. record a score
   for each key of how confident we are that we found the plaintext. -}
tryCrack :: String -> KeySpace -> [Int] -> [Int]
tryCrack key [] cipherText = []
tryCrack key keyspace cipherText = (decipherAndScore key cipherText) :
  (tryCrack (head keyspace) (tail keyspace) cipherText)

{- find the index of the first, maximum value of a list of Ints -}
maxIndex' :: Ord a => [a] -> Int -> Int -> Int
maxIndex' vals pos _maxIndex
  | pos > (length vals) - 1 =
    _maxIndex
  | (vals !! pos) > (vals !! _maxIndex) =
    maxIndex' vals (pos + 1) pos
  | otherwise =
    maxIndex' vals (pos + 1) _maxIndex

{- find the index of the first, maximum value of a list of Ints. 
   public interface -}
maxIndex :: Ord a => [a] -> Int
maxIndex vals = maxIndex' vals 1 0

{- crack a given ciphertext with a given keyspace -}
crack :: String -> KeySpace -> (String, String, Int)
crack sCipherText keyspace = 
  let cipherText = (splitInts sCipherText)
      winnerIndex = maxIndex (tryCrack (head keyspace) (tail keyspace) cipherText)
      winner = keyspace !! winnerIndex
      plainText = decipher winner cipherText 0
  in (winner, plainText, strSum plainText)


