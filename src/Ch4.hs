--
-- Chapter 4 of Haskell Programming, Christopher Allen
--

module Ch4 where

import qualified Data.Text as Text
import qualified Data.Char as Char
import Data.Tuple

swapTuple :: (String, Int)
swapTuple = swap (1 :: Int, "ahmed")

fst' :: (t1, t) -> t1
fst' (a, b) = a

snd' :: (t, t1) -> t1
snd' (a, b) = b

-- Chapter Exercises

awesome :: [String]
awesome = ["Papuchon", "curry", ":)"]

also :: [String]
also = ["Quake", "The Simons"]

allAwesome :: [[String]]
allAwesome = [awesome, also]

-------

-- Write a function that tells you whether or not a given String (or list)
-- is a palindrome.  Here you'll want to use a function called reverse.

-- drop non characters from a string
dropNonChars :: String -> String
dropNonChars str = filter (\x -> x `elem` ['a'..'z'] || x `elem` ['A'..'Z']) str

-- isPalindrome "A dog! A panic in a pagoda!"
isPalindrome :: String -> Bool
isPalindrome x = x' == (reverse x')
  where x' = map (Char.toLower) $ dropNonChars x

-------

myAbs :: Integer -> Integer
myAbs x = if (x < 0) then (-1) * x else x

-- Fill in the defintion of the following function using fst and snd
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f t1 t2 = ((snd t1, snd t2), (fst t1, fst t2))

------

-- Here we want a function that adds 1 to the length of a string
-- argument and returns that result.
x = (+)
g xs = w `x` 1
  where w = length xs

myId :: t -> t
myId x = x

-- This function will return 1 from the value (1, 2)
h (a, b) = a


