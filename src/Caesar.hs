module Caesar (
  caesar,
  uncaesar
  ) where

import Data.Char

caesar :: Int -> String -> String
caesar n s = map (rotate n) s

uncaesar :: Int -> String -> String
uncaesar n s = map (rotate (-n)) s

-- shift the character 'c' to the right by n
rotate :: Int -> Char -> Char
rotate n c
  | c `elem` alphabet = chr $ (ord c + n - ord 'a') `mod` 26 + ord 'a'
  | otherwise = c

alphabet = "abcdefghijklmnopqrstuvwxyz"

