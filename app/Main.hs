module Main (main) where

import qualified Data.List as List

--
-- C-c C-z switch between repl back and forth
--

main :: IO ()
main = do
  let xs = [1.0, 2.0, 3.0]
  let r = f xs
  print r
  print (sqrt_ $ fromIntegral 9)
  putStrLn "done main"

maxInt :: Int
maxInt = maxBound :: Int

minInt :: Int
minInt = minBound :: Int

sqrt_ :: Floating a => a -> a
sqrt_ x = sqrt x

f :: [Double] -> Double
f lst = sum (map (3*) lst)

evenList :: Integral a => [a]
evenList = [2, 4..20]

infiniPow10 :: [Integer]
infiniPow10 = [10,20..]

cycleList :: [Integer]
cycleList = take 10 (cycle [1,2,3,4,5])

listTimes2 :: [Integer]
listTimes2 = [x * 2 | x <- [1..10]]

listTimes3 :: [Integer]
listTimes3 = [x * 3 | x <- [1..20], x * 3 <= 50]

divisibleByNineThirteen :: [Integer]
divisibleByNineThirteen = [x | x <- [1..500], x `mod` 13 == 0, x `mod` 9 == 0]

sortedList :: [Integer]
sortedList = List.sort [9, 2, 3, 5, 17]

sumOfLists :: [Integer]
sumOfLists = zipWith (+) [1,2,3,4,5] [6,7,8,9,10]

biggerThan5 :: [Integer]
biggerThan5 = filter (> 5) [1..10]

evensUpTo20 :: [Integer]
evensUpTo20 = takeWhile (<= 20) [2,4..100]

multOfList :: Integer
multOfList = foldl (*) 1 [1,2,3,4,5]

multTable :: [[Integer]]
multTable = [[x * y | y <- [1..10]] | x <- [1..10]]

names = ["Ahmed", "Bob", "John"]
addresses = ["7 Main", "234 North", "567 South"]

namesAddresses :: [(String, String)]
namesAddresses = zip names addresses

-- as pattern
getFirstItem :: String -> String
getFirstItem [] = "Empty String"
getFirstItem all@(x:xs) = "The first letter in " ++ all ++ " is " ++ [x] ++ ", rest is " ++ xs
