import Data.Char
import Data.List

{--

Remember old-fashioned phone inputs for writing text where
you had to press a button multiple times to get different letters to
come up? You may still have to do this when you try to search for a
movie to watch using your television remote control. You’re going
to write code to translate sequences of button presses into strings
and vice versa.
So! Here is the layout of the phone:

-----------------------------------------
|  1      |  2 ABC  | 3 DEF   |
_________________________________________
|  4 GHI  |  5 JKL  | 6 MNO   |
-----------------------------------------
|  7 PQRS |  8 TUV  | 9 WXYZ  |
-----------------------------------------
|  * ^    |  0 + _  | # .,    |
-----------------------------------------
--}

-- Where star (*) gives you capitalization of the letter you’re writing
-- to your friends, and 0 is your space bar. To represent the digit itself,
-- you press that digit once more than the letters it represents. If you
-- press a button one more than is required to type the digit, it wraps
-- around to the first letter. For example,

-- 2     -> 'A'
-- 22    -> 'B'
-- 222   -> 'C'
-- 2222  -> '2'
-- 22222 -> 'A'

-- 1. Create a data structure that captures the phone layout above.
-- The data structure should be able to express enough of how the
-- layout works that you can use it to dictate the behavior of the
-- functions in the following exercises.

data PhoneKey = PhoneKey Char [Char] deriving Show

-- fill in the rest.
data DaPhone = DaPhone [PhoneKey] deriving Show

phone = DaPhone [
  PhoneKey '1' [],
  PhoneKey '2' ['A','B','C'],
  PhoneKey '3' ['D','E','F'],
  PhoneKey '4' ['G','H','I'],
  PhoneKey '5' ['J','K','L'],
  PhoneKey '6' ['M','N','O'],
  PhoneKey '7' ['P','Q','R', 'S'],
  PhoneKey '8' ['T','U','V'],
  PhoneKey '9' ['W','X','Y','Z'],
  PhoneKey '*' ['^'],
  PhoneKey '0' [' '],
  PhoneKey '#' ['.']
  ]

-- 2. Convert the following conversations into the keypresses re-
-- quired to express them. We’re going to suggest types and func-
-- tions to fill in order to accomplish the goal, but they’re not
-- obligatory. If you want to do it differently, go right ahead.

convo :: [String]
convo =
  ["Wanna play 20 questions",
   "Ya",
   "U 1st haha",
   "Lol ok. Have u ever tasted alcohol",
   "Lol ya",
   "Wow ur cool haha. Ur turn",
   "Ok. Do u think I am pretty Lol",
   "Lol ya",
   "Just making sure rofl ur turn"
  ]

-- validButtons = "1234567890*#"
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int

-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]
reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone []) c = []
reverseTaps (DaPhone ((PhoneKey d lst) : xs)) c
  | c == d = [(d, (length lst + 1))]
  | (toUpper c) `elem` lst = if (isUpper c) then [('*', 1), (d, indexOf (toUpper c) lst)]
                             else [(d, indexOf (toUpper c) lst)]
  | otherwise = reverseTaps (DaPhone xs) c

indexOf :: Eq a => a -> [a] -> Int
indexOf x ys = go x ys 1
  where
    go :: Eq a => a -> [a] -> Int -> Int
    go x (y:ys) acc = case x == y of
      True -> acc
      otherwise -> go x ys (acc + 1)

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone str = concat $ map (reverseTaps phone) str

-- 3. How many times do digits need to be pressed for each message?
fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps xs = foldr (\(_, p1) p2 -> p1 + p2) 0 xs

-- Example
taps = fingerTaps $ cellPhonesDead phone $ convo !! 6

tapsForChar :: DaPhone -> Char -> Presses
tapsForChar phone = fingerTaps . reverseTaps phone

-- Find the maximum pair in a list of pairs
-- where equality is based on the first element of the pair
maximum' :: [(Int, String)] -> (Int, String)
maximum' xs = go xs (head xs)
  where
    go [] p2 = p2
    go (p1 : xs) (p2) = if (fst p1 > fst p2) then go xs p1 else go xs p2

testMaximum' = maximum' [(1, "aa"), (2, "bb")] == (2, "bb")

sortAndDropSpaces = dropWhile (== ' ') . sort

-- 4. What was the most popular letter for each message? What was
-- its cost? You’ll want to combine reverseTaps and fingerTaps to
-- figure out what it cost in taps. reverseTaps is a list because you
-- need to press a different button in order to get capitals.
mostPopularLetter :: String -> Char
mostPopularLetter str = head . snd $ maximum' $ map (\a -> (length a, a)) (group' $ sortAndDropSpaces str)

-- TODO
-- Implementation of group
-- group' [1,2,2,1,3,3,2] = [[1],[2,2],[1],[3,3],[2]]
group' :: Eq a => [a] -> [[a]]
group' []     = []
group' (x:xs) = (x : prefix) : group' remainder
  where (prefix, remainder) = span (== x) xs

group_ (x:xs) = (x : takeWhile (== x) xs) : group' (dropWhile (== x) xs)

testGroup' = group' [1,2,2,1,3,3,2] == [[1],[2,2],[1],[3,3],[2]]

-- 5. What was the most popular letter overall? What was the most
-- popular word?
coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . (map mostPopularLetter)

mostPopularWord :: String -> String
mostPopularWord str = head . snd $ maximum $ map (\a -> (length a, a)) (group $ sort $ words str)

nthMostPopularWord :: String -> Int -> String
nthMostPopularWord str n = head . snd $ head $ drop n $
  sortBy (\a b -> if (a > b) then LT else GT) $ map (\a -> (length a, a)) (group $ sort $ words str)

coolestWord :: [String] -> String
coolestWord = mostPopularWord . (concat . intersperse " ")

main :: IO ()
main = do
  -- str <- readFile "/home/parallels/Work/haskell/haskell-programming/1399-0.txt"
  str <- readFile "/home/parallels/Work/haskell/haskell-programming/pg1513.txt"
  
  print $ "Number of words = " ++ show ((length . words) str)

  let popular = map (nthMostPopularWord str) [0..12]
  mapM_ putStrLn popular
  
  return ()
