module Vigenere where

import Data.Char

--
-- In the Lists chapter, you wrote a Caesar cipher. Now, we want to
-- expand on that idea by writing a Vigenère cipher. A Vigenère cipher is
-- another substitution cipher, based on a Caesar cipher, but it
-- uses a series of Caesar ciphers for polyalphabetic substitution. The
-- substitution for each letter in the plaintext is determined by a fixed
-- keyword.
--
-- So, for example, if you want to encode the message “meet at dawn,”
-- the first step is to pick a keyword that will determine which
-- Caesar cipher to use. We’ll use the keyword “ALLY” here. You repeat
-- the keyword for as many characters as there are in your original message:
--
-- MEET AT DAWN
-- ALLY AL LYAL
--
-- Now the number of rightward shifts to make to encode each
-- character is set by the character of the keyword that lines up with it.
-- The ’A’ means a shift of 0, so the initial M will remain M. But the ’L’
-- for our second character sets a rightward shift of 11, so ’E’ becomes
-- ’P’. And so on, so “meet at dawn” encoded with the keyword “ALLY”
-- becomes “MPPR AE OYWY.”
--
-- A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z |
-- 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10| 11| 12| 13| 14| 15| 16| 17| 18| 19| 20| 21| 22| 23| 24| 25|

alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

msg = "MEET AT DAWN"
key = "ALLY"

shiftMessage :: String -> [Int]
shiftMessage msg = undefined

f msg key = res
  where
    -- list of tuple of (char,index) of the msg
    -- e.g. if msg = 'AHM', then
    -- posTuples = [('A', 0), ('H', 1), ('M', 2)]
    -- Note that we filter out spaces from the msg, since spaces are not encoded.
    posTuples = zip (filter (/= ' ') msg) [0..length msg]
    -- Just the positions themselves
    pos = map snd posTuples

    -- For each index, get the corresponding letter of the key
    keyCodes = map (rotateKey key) pos

    -- For each char in keyCodes, get their alphabetic position.
    -- These positions will be the values by which we need to shift the characters of the
    -- msg.

    res = map getShift keyCodes
                   
-- Get the character at position n in the given key
-- If n > length of the key, then wrap around the key
rotateKey :: String -> Int -> Char
rotateKey key n = c
  where
    len = length key
    c = key !! (n `mod` len)

getShift :: Char -> Int
getShift c = ord (toUpper c) - ord 'A'
