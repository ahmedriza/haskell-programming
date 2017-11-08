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

alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

-- Encode the given message
encode msg key = res
  where
    msg' = map toUpper msg
    alignments = keyAlignment key msg' 0
    shifts = zip msg' $ map getPosition alignments
    res = map rotate shifts

-- Decode an encoded message with the same key
decode enc key = undefined

--------------------------------------------------------------------------------

rotate :: (Char, Int) -> Char
rotate (c, n) = case c `elem` alphabet of
  True -> alphabet !! newPosition
    where
      pos = getPosition (toUpper c)
      newPosition = (pos + n) `mod` (length alphabet)
  otherwise -> c

-- Get the position of the given char c in the alphabet, 'A' = 0, ..., 'Z' = 25
-- If the character is not part of alphabet, then return 0.
getPosition :: Char -> Int
getPosition c = case c `elem` alphabet of
                  True -> ord (toUpper c) - ord 'A'
                  otherwise -> 0

-- Align the original msg with characters from the key
--
-- MEET AT DAWN
-- ALLY AL LYAL
--
keyAlignment :: String -> String -> Int -> String
keyAlignment key  [] _ =[]
keyAlignment key (x:xs) n = case x `elem` alphabet of
  True      -> rotateKey key n : keyAlignment key xs (n+1)
  otherwise -> x : keyAlignment key xs n

-- Get the character at position n in the given key
-- If n > length of the key, then wrap around the key
rotateKey :: String -> Int -> Char
rotateKey key n = key !! (n `mod` (length key))


test1 :: IO ()
test1 = do
  let msg_ = "MEET AT DAWN"
      key_ = "ALLY"
      result_ = "MPPR AE OYWY"
      outcome = case encode msg_ key_ == result_ of
                  True -> "Success"
                  False -> "Failed"
  print outcome

test2 :: IO ()
test2 = do
  let msg_ = "ATTACKATDAWN"
      key_ = "LEMON"
      result_ = "LXFOPVEFRNHR"
      outcome = case encode msg_ key_ == result_ of
                  True -> "Success"
                  False -> "Failed"
  print outcome
