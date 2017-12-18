--
-- Chapter 9
--

import Data.Char
import Data.Bool
import Debug.Trace

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x : []) = Nothing
safeTail (_ : xs) = Just xs

--------------------------------------------------------------------------------

-- Implementation of enumFromTo for certain types

eft :: (Ord a, Enum a) => a -> a -> [a] -> [a]
eft a b acc
  | a > b = []
  | a == b = reverse $ (a : acc)
  | otherwise = eft (succ a) b (a : acc)

eftBool :: Bool -> Bool -> [Bool]
eftBool a b = eft a b []

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd a b = eft a b []

eftInt :: Int -> Int -> [Int]
eftInt a b = eft a b []

eftChar :: Char -> Char -> [Char]
eftChar a b = eft a b []

--------------------------------------------------------------------------------

-- Using takeWhile and dropWhile write a function that takes a string and returns
-- a list of strings using spaces to separate the elements of the strings into
-- words, as in the following example
--- e.g. myWords "sheryl wants fun" = ["sheryl", "wants", "fun"]

myWords' [] = []
myWords' (' ' : s) = myWords' s
myWords' s = f : myWords' r
  where
    f = takeWhile (/= ' ') s
    r = dropWhile (/= ' ') s

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines' [] = []
myLines' ('\n' : s) = myLines' s
myLines' s = f : myLines' r
  where
    f = takeWhile (/= '\n') s
    r = dropWhile (/= '\n') s


shouldEqual = [
  "Tyger Tyger, burning bright",
  "In the forests of the night",
  "What immortal hand or eye",
  "Could frame thy fearful symmetry"
  ]

stringToList :: Char -> String -> [String]
stringToList sep [] = []
stringToList sep s = f : stringToList sep r
  where
    f = takeWhile (/= sep) $ dropWhile (== sep) s
    r = dropWhile (/= sep) $ dropWhile (== sep) s

myWords = stringToList ' '

myLines = stringToList '\n'

-- List Comprehensions

-- Pythagorean Triples

pyth n = [ (x, y, z) | z <- [1..n], y <- [1..z], x <- [1..y], x^2 + y^2 == z^2 ]

xy = [ x^y | x <- [1..10], y <- [2,3], x^y < 200 ]

pairs = [ (a, b) | a <- [1..5], b <- ['a', 'b'] ]


-- Exercises: Comprehend Thy Lists

mysq = [ x^2 | x <- [1..5] ]
mycube = [ y^3 | y <- [1..5] ]

evenSq = [x | x <- mysq, rem x 2 == 0]

mystery = [(x, y) | x <- mysq, y <- mysq, x < 50, y > 50]

mystery' = take 5 [(x, y) | x <- mysq, y <- mysq, x < 50, y > 50]

-- List Comprehension With Strings

uppers = [x | x <- "Three Letter Acronym", x `elem` ['A'..'Z']]

acronymBuilder xs = [x | x <- xs, elem x ['A'..'Z']]

vowelBuilder xs = [x | x <- xs, elem x ['a','e','i','o','u']]

-- Write an expression that will make tuples of the outputs of mysq and mycube
tup = [ (a,b) | a <- mysq, b <- mycube]

-- Now alter the expresison so that it only uses the x and y values that are
-- less than 50
tup' = [ (a,b) | a <- mysq, b <- mycube, a < 50, b < 50]

-- apply another function to that list comprehension to determine how many
-- tuples inhabit your output list

-- length tup' or length tup


-- Length is strict in the spine of the list, but not in the values
length' [] = 0
length' (_:xs) = 1 + length xs


f :: [Char] -> [Bool]
f xs = map (\x -> elem x "aeiou") xs

-- bool x y p
-- if p then y else x
-- if (x==3) then (-x) else x
g = map (\x -> bool x (-x) (x==3)) [1,2,3,0,3]


filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' pred (x:xs)
  | pred x = x : filter' pred xs
  | otherwise = filter' pred xs

-- Multiples of 3 in the list of numbers from 1 to 30

-- mult3 = [x | x <- [1..30], rem x 3 == 0]
mult3 = filter (\x -> rem x 3 == 0) [1..30]

-- Length of the above list

mult3Len = length . filter (\x -> rem x 3 == 0) $ [1..30]

nonArticles :: String -> [String]
nonArticles s = filter (\x -> x /= "the" && x /= "a" && x /= "an") $ words s

h = zipWith (+) [1..10] [11..20]

-- Write your own version of zip

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

-- Write your own version of zipWith

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- Rewrite zip' in terms of zipWith'

zip'' = zipWith' (\a b -> (a,b))

-- Chapter Exercises

filterUppers :: [Char] -> [Char]
filterUppers s = filter isUpper s

firstCaps :: String -> String
firstCaps [] = []
firstCaps (x:xs) = (toUpper x) : xs

allCaps :: String -> String
allCaps [] = []
allCaps (x:xs) = (toUpper x) : allCaps xs

firstCap :: String -> Char
firstCap  = toUpper . head
