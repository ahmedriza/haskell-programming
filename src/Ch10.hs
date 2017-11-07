--
-- Chapter 10
--

import Data.Time

data DatabaseItem = DbString String
                    | DbNumber Integer
                    | DbDate UTCTime
                    deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]

tmpDb = DbNumber 1

-- 1. Write a function that filters for DbDate values and returns a list of the UTCTime values inside them.

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate items = foldr go [] items
  where
    go :: DatabaseItem -> [UTCTime] -> [UTCTime]
    go item acc = case item of
      (DbDate utcTime) -> utcTime : acc
      otherwise        -> acc 

filterDbDate' items = [utcTime | (DbDate utcTime) <- items ]

xs = [1,2] ++ undefined

-- take 2 $ foldr (:) [] xs = [1,2]

-- foldr (:) [] xs
-- (:) 1 (foldr (:) [] $ [2] ++ undefined)
--       (:) 2 (foldr (:) [] undefined)
--

foldr'' f acc [] = acc
foldr'' f acc (x:xs) = f x (foldr f acc xs)

foldl'' f acc [] = acc
foldl'' f acc (x:xs) = foldl'' f (f acc x) xs

-- take 2 $ foldl (flip (:)) [] xs = error

-- foldl (flip (:)) [] xs
-- foldl (flip (:)) ((flip (:)) [] 1) $ [2] ++ undefined
-- foldl (flip (:)) [1] $ [2] ++ undefined
-- foldl (flip (:)) ((flip (:)) [1] 2) undefined
-- foldl (flip (:)) [2,1] undefined
-- foldl (flip (:)) ((flip (:)) [2,1] ???)


-- =============================
-- Evaluation of foldr in Detail
-- =============================

eg1 = foldr const 0 [1..5]

-- const 1 (foldr const 0 [2,3,4,5])
-- = 1


eg2 = foldl (flip const) 0 [undefined,5]
--
-- This will evaluate eventually to
-- (flip const) (...) 5
-- It will ignore the stuff in (...) and return 5
-- 
-- Here's how it works:
-----------------------
-- foldl (flip const) ((flip const) 0 undefined) [5]
--                    ^^^^^^^^^^^^^^^^^^^^^^^^^^
--                          acc

-- foldl (flip const) ((flip const) ((flip const) 0 undefined) 5) []
--                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
--                         acc
-- ==
-- (flip const) ((flip const) 0 undefined) 5
--               ^^^^^^^^^^^^^^^^^^^^^^^^^
-- ==
-- 5


-- For finite lists:
--
-- foldr f z xs = foldl (flip f) z (reverse xs)

test = (foldr (:) [] [1,2,3]) == (foldl (flip (:)) [] $ reverse [1,2,3])

plus = (+)
--
-- scanr plus 0 [1,2,3] == [6, 5, 3, 0]
--
--
-- foldr plus 0 [1,2,3]
-- plus 1 (foldr plus 0 [2,3])
-- plus 1 plus 2 (foldr plus 0 [3])
-- plus 1 plus 2 plus 3 (foldr plus 0 [])
-- (plus 1 (plus 2 (plus 3 0)))

-- scanl (+) 0 [1,2,3] == [0, 1, 3, 6]
--
-- foldl (+) 0 [1,2,3]
-- foldl plus (plus 0 1) [2,3]
-- foldl plus (plus (plus 0 1) 2) [3]
-- foldl plus (plus (plus (plus 0 1) 2) 3) []
-- (plus (plus (plus 0 1) 2) 3)

scanl' :: (b -> a -> b) -> b -> [a] -> [b]
scanl' f acc ls =
  acc : (case ls of
         [] -> []
         x:xs -> scanl' f (f acc x) xs)

-- scanl plus 0 [1,2,3]
-- 0 : scanl plus (plus 0 1) [2,3]
--     1 : scanl plus (plus 1 2) [3]
--         3 : scanl plus (plus 3 3) []
--             6 : []
-- 0 : 1 : 3 : 6 : []

fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

fibsN :: Int -> Integer
fibsN x = fibs !! x

-- 1: scanl plus 1 fibs
-- 1: scanl plus 1 (1 : scanl (+) 1 fibs)
--                  ^   ^^
--                  x : xs
--
-- 1: 1: scanl plus (plus 1 1) (scanl plus 1 fibs)
--                  ^^^^^^^^^^
--              f     acc         ls
--       2 : scanl plus (plus 2 


-- Write mMap in terms of foldr. It should have the same behaviour as the built-in map
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) [] 

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = conc x (squish xs)

-- concatenate two lists
conc :: [a] -> [a] -> [a]
conc [] ys = ys
conc (x:xs) ys = x : conc xs ys

squish' :: Foldable t => t [a] -> [a]
squish' = foldr conc [] 

-- squishMap maps a function over a list and concatenates the results.
-- e.g. squishMap (\x -> [1, x, 3]) [2] = [1,2,3]

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish' . myMap f

-- myMaximumBy takes a comparison function and a list and returns the
-- greatest element of the list based on the last value that the
-- comparison returned GT for.

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f [] = error "empty list"
myMaximumBy f (x:xs) = go f x xs
  where
    go f a [] = a
    go f a (b:bs) = case f a b of
                      GT -> go f a bs
                      otherwise -> go f b bs

myMaximumBy' f (x:xs) = foldr (\a b -> if f a b == GT then a else b) x (x:xs)

myMinimumBy' f (x:xs) = foldr (\a b -> if f a b == GT then b else a) x (x:xs)






