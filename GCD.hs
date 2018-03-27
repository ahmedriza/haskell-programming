module GCD where

a = 198
b = 168

gcd' :: Integer -> Integer -> Integer
gcd' a b
  | b == 0 = a
  | otherwise = gcd' b (a `mod` b)
