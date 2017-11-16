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

alpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
msg_ = "MEET AT DAWN"
key_ = "ALLY"

-------------------------------------------------------------------------------------
------- MEET AT DAWN
------- ALLYALLYALLY
-------------------------------------------------------------------------------------

ff :: String -> String -> [Int]
ff msg key = map ord $ take (length msg) $ cycle key

gg msg key = zip (map ord msg) (ff msg key)

hh :: (Int, Int) -> Int
hh (a, b) = (a + b) `mod` 128

enc' msg_ key_ = map chr (map hh (gg msg_ key_))

-------------------------------------------------------------------------------------

kk :: (Int, Int) -> Int
kk (a, b) = (a - b) `mod` 128 -- if (a < b) then (128 + a - b) else (a - b)

dec' msg key = map chr $ map kk $ zip (map ord msg) (ff msg key)

-------------------------------------------------------------------------------------

d = [(77,65),(69,76),(69,76),(84,89),(32,65),(65,76),(84,76),(32,89),(68,65),(65,76),(87,76),(78,89)]
e = [(14,65),(17,76),(17,76),(45,89),(97,65),(13,76),(32,76),(121,89),(5,65),(13,76),(35,76),(39,89)]

msg__ = "ATTACKATDAWN"
key__ = "LEMON"
result__ = "LXFOPVEFRNHR"

