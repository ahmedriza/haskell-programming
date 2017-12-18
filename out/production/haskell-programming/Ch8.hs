--
-- Chapter 8 of Haskell Programming, Christopher Allen
--
module Ch8 where

import Data.List (intersperse)

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

type Numerator = Integer
type Denominator = Integer
type Quotient = Integer
type Remainder = Integer

divideBy :: Numerator -> Denominator -> (Quotient, Remainder)
divideBy n d = go n d 0
  where go n d count
          | d <= 0 = error "divisor must be >= 0"
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)

func :: [a] -> [a] -> [a]
func x y = x ++ y

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy = flip cattyConny

appendCatty = cattyConny "woops"

frappe = flippy "haha"

-- Write a function that recursively sums all numbers from 1 to n
sum' :: (Eq a, Num a) => a -> a
sum' 0 = 0
sum' n = n + sum' (n - 1)

-- Write a function that multiplies two integral numbers using recursive
-- summation. The type should be (Integral a) => a -> a -> a
mult' :: (Integral a) => a -> a -> a
mult' a 0 = 0
mult' a b = a + mult' a (b - 1)

-- McCarthy 91 function

-- MC(n) = n - 10, if n > 100
--       = MC(MC(n + 11)) if n <= 100

mc91 n
  | n > 100 = n - 10
  | n <= 100 = (mc91 . mc91) (n + 11)


digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"

digits :: Int -> [Int]
digits 0 = [0]
digits n = go (n `divMod` 10) []
  where
    go :: (Int, Int) -> [Int] -> [Int]
    go (0, 0) acc = acc
    go (x, y) acc = go (x `divMod` 10) (y : acc)

wordNumber :: Int -> String
wordNumber n = concat . intersperse "-" $ map digitToWord (digits n)


