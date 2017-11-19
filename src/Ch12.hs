--
-- Chapter 12
--

import Data.List

ifEvenAdd2 :: Integer -> Maybe Integer
ifEvenAdd2 n = if even n then Just (n + 2) else Nothing

type Name = String
type Age = Integer
type ValidatePerson a = Either [PersonInvalid] a

data Person = Person Name Age deriving Show

------------------------------------------------------------------------------------

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   deriving (Show)

instance Eq PersonInvalid where
  (==) NameEmpty NameEmpty = True
  (==) AgeTooLow AgeTooLow = True
  (==) _ _ = False

blah :: PersonInvalid -> String
blah pi
  | pi == NameEmpty = "Name empty"
  | pi == AgeTooLow = "Age too low"
  | otherwise       = "???"

ageOkay :: Age -> ValidatePerson Age
ageOkay age = case age >= 0 of
  True -> Right age
  False -> Left [AgeTooLow]

nameOkay :: Name ->  ValidatePerson Name
nameOkay name = case name /= "" of
  True -> Right name
  False -> Left [NameEmpty]

mkPerson ::  Name -> Age -> ValidatePerson Person
mkPerson name age = mkPerson'  (nameOkay name) (ageOkay age)

mkPerson' :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
mkPerson' (Right name) (Right age)     = Right (Person name age)
mkPerson' (Left badName) (Left badAge) = Left (badName ++ badAge)
mkPerson' (Left badName) _             = Left badName
mkPerson' _ (Left badAge)              = Left badAge

------------------------------------------------------------

data Unary a = Unary a deriving Show

--------------------
-- Chapter Exercises
--------------------

notThe :: String -> Maybe String
notThe str = case str of
  "the" -> Nothing
  otherwise -> Just str

fromMaybe' :: a -> Maybe a -> a
fromMaybe' a (Just b) = b
fromMaybe' a (Nothing) = a

-- replaceThe "the cow loves us"
-- "a cow loves us"
replaceThe :: String -> String
replaceThe str = res
  where
    w = map (fromMaybe' "a") $ map notThe (words str)
    res = concat $ intersperse " " w

-- Write a recursive function that takes a text/string, breaks it into
-- words, and counts the number of instances of ”the” followed by
-- a vowel-initial word.
--
-- countTheBeforeVowel "the cow"
-- 0
-- countTheBeforeVowel "the evil cow"
-- 1
-- "the air show with the owl"
-- 2
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel str = foldr f 0 (zip w (tail w))
  where
    w = words str
    f :: (String, String) -> Integer -> Integer
    f (s1, (s : s2)) acc
      | s1 == "the" && s `elem` "aeiou" = 1 + acc
      | otherwise                       = acc

-- Return the number of letters that are vowels in a word.
-- Hint: it’s helpful to break this into steps. Add any helper func-
-- tions necessary to achieve your objectives.
-- a) Test for vowelhood
-- b) Return the vowels of a string
-- c) Count the number of elements returned
-- countVowels "the cow"
-- 2
-- countVowels "Mikolajczak"
-- 4
countVowels :: String -> Integer
countVowels  = fromIntegral . length . filter isVowel

isVowel :: Char -> Bool
isVowel c = c `elem` vowels

vowels = "aeiou"

--Validate the word
-------------------
-- Use the Maybe type to write a function that counts the number of
-- vowels in a string and the number of consonants. If the number
-- of vowels exceeds the number of consonants, the function returns
-- Nothing. In many human languages, vowels rarely exceed the number
-- of consonants so when they do, it may indicate the input isn’t a word
-- (that is, a valid input to your dataset):

newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord str
  | numVowels > numConsonants = Nothing
  | otherwise                 = Just (Word' str)
  where
    numVowels = countVowels str
    numConsonants = (fromIntegral . length) str - numVowels


-- You’ll be presented with a datatype to represent the natural numbers.
-- The only values representable with the naturals are whole numbers
-- from zero to infinity. Your task will be to implement functions to
-- convert Naturals to Integers and Integers to Naturals. The conversion
-- from Naturals to Integers won’t return Maybe because Integer is a strict
-- superset of Natural. Any Natural can be represented by an Integer,
-- but the same is not true of any Integer. Negative numbers are not
-- valid natural numbers.

data Nat = Zero
         | Succ Nat deriving (Eq, Show)

-- natToInteger Zero
-- 0
-- natToInteger (Succ Zero)
-- 1
-- natToInteger (Succ (Succ Zero))
-- 2
natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat n
  | n < 0 = Nothing
  | otherwise = Just (go n)
  where
    go 0 = Zero    
    go n = Succ (go (n-1))

----

-- Simple boolean checks for Maybe values
-- >>> isJust (Just 1)
-- True
-- >>> isJust Nothing
-- False
isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

-- >>> isNothing (Just 1)
-- False
-- >>> isNothing Nothing
-- True
isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing (Just _) = False

-- The following is the Maybe catamorphism. You can turn a Maybe
-- value into anything else with this.
-- >>> mayybee 0 (+1) Nothing
-- 0
-- >>> mayybee 0 (+1) (Just 1)
-- 2
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee acc f Nothing = acc
mayybee acc f (Just a) = mayybee (f a) f Nothing

-- In case you just want to provide a fallback value
-- Try writing it in terms of the maybe catamorphism
-- >> fromMaybe 0 Nothing
-- 0
-- >>> fromMaybe 0 (Just 1)
-- 1
fromMaybe :: a -> Maybe a -> a
fromMaybe a b = mayybee a id b

-- Converting between List and Maybe.
-- >>> listToMaybe [1, 2, 3]
-- Just 1
-- >>> listToMaybe []
-- Nothing
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

-- >>> maybeToList (Just 1)
-- [1]
-- >>> maybeToList Nothing
-- []
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

-- For when we want to drop the Nothing values from our list.
-- >>> catMaybes [Just 1, Nothing, Just 2]
-- [1, 2]
-- >>> let xs = take 3 $ repeat Nothing
-- >>> catMaybes xs
-- []
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing : xs) = catMaybes xs
catMaybes (Just x : xs) = x : catMaybes xs
  
-- You’ll see this called “sequence” later.
-- >>> flipMaybe [Just 1, Just 2, Just 3]
-- Just [1, 2, 3]
-- >>> flipMaybe [Just 1, Nothing, Just 3]
-- Nothing
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe (Nothing : _) = Nothing
flipMaybe (Just x : xs) = case flipMaybe xs of
  Just ys -> Just (x:ys)
  Nothing -> Nothing

-- Small Library for Either

-- Try to eventually arrive at a solution that uses foldr, even if
-- earlier versions don’t use foldr.

lefts' :: [Either a b] -> [a]
lefts' xs = foldr f [] xs
  where
    f :: Either a b -> [a] -> [a]
    f (Left a) acc = a : acc
    f (Right _) acc = acc

rights' :: [Either a b] -> [b]
rights' xs = foldr f [] xs
  where
    f :: Either a b -> [b] -> [b]
    f (Left _) acc = acc
    f (Right b) acc = b : acc

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = foldr f ([],[]) xs 
  where
    f :: Either a b -> ([a], [b]) -> ([a],[b])
    f (Left a) (as, bs) = (a:as, bs)
    f (Right b) (as, bs) = (as, b:bs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right b) = Just (f b)

--  This is a general catamorphism for Either values.
either' :: (a -> c)
        -> (b -> c)
        -> Either a b
        -> c
either' f _ (Left a) = f a
either' _ g (Right b) = g b

-- Same as before, but use the either' function you just wrote.
eitherMaybe'' :: (b -> c)
              -> Either a b
              -> Maybe c
eitherMaybe'' f v = either' (const Nothing) (Just . f) v


-- Unfolds

sample = take 10 $ iterate (+1) 0

-- unfoldr :: (b -> Maybe (a, b)) -> b -> [a]

sample' = take 10 $ unfoldr (\b -> Just (b, b+1)) 0

mehSum :: Num a => [a] -> a
mehSum xs = go xs 0
  where
    go [] acc = acc
    go (x:xs) acc = go xs (x+acc)

niceSum :: Num a => [a] -> a
niceSum xs = foldl' (+) 0 xs

mehProduct :: Num a => [a] -> a
mehProduct xs = go xs 1
  where
    go [] acc = acc
    go (x:xs) acc = go xs (x*acc)

niceProduct :: Num a => [a] -> a
niceProduct = foldl' (*) 1

mehConcat :: [[a]] -> [a]
mehConcat xs = go xs []
  where
    go [] acc = acc
    go (x:xs) acc = go xs (acc ++ x)

niceConcat :: [[a]] -> [a]
niceConcat = foldr (++) []

------------------------

-- Write the function myIterate using direct recursion. Compare
-- the behavior with the built-in iterate to gauge correctness. Do
-- not look at the source or any examples of iterate so that you
-- are forced to do this yourself.

myIterate :: (a -> a) -> a -> [a]
myIterate f n = n : myIterate f (f n)

testMyIterate = take 10 $ myIterate (+1) 0

-- Write the function myUnfoldr using direct recursion. Compare
-- with the built-in unfoldr to check your implementation. Again,
-- don’t look at implementations of unfoldr so that you figure it
-- out yourself.

myUnfoldr :: (b -> Maybe (a, b))
          -> b
          -> [a]
myUnfoldr f b = case f b of
  Nothing      -> []
  Just (a, b') -> a : myUnfoldr f b'

testMyUnfoldr = (take 10 $ unfoldr (\b -> Just (b*2, b+2)) 0) ==
  (take 10 $ myUnfoldr (\b -> Just (b*2, b+2)) 0)


-- Rewrite myIterate into betterIterate using myUnfoldr. A hint —
-- we used unfoldr to produce the same results as iterate earlier.
-- Do this with different functions and see if you can abstract the
-- structure out.
--
-- Remember, your betterIterate should have the same results as
-- iterate.

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\a -> Just (a, f a)) 
  
testBetterIterate = take 10 $ betterIterate (+1) 0

--------------------------------------
-- Finally something other than a list
--------------------------------------

data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
                  deriving (Eq, Ord, Show)

-- Write unfold for BinaryTree
unfold :: (a -> Maybe (a, b, a))
       -> a
       -> BinaryTree b
unfold = undefined

-- Make a tree builder
-- Using the unfold function you've made for BinaryTree, write the following function:
treeBuild :: Integer -> BinaryTree Integer
treeBuild n = undefined

-- You should be producing results that look like the following

-- treeBuild 0
-- Leaf

-- treeBuild 1
-- Node Leaf 0 Leaf

-- treeBuild 2
-- Node (Node Leaf 1 Leaf)
-- 0
-- (Node Leaf 1 Leaf)

-- treeBuild 3
-- Node (Node (Node Leaf 2 Leaf)
--            1
--            (Node Leaf 2 Leaf))
--      0
--      (Node (Node Leaf 2 Leaf)
--            1
--            (Node Leaf 2 Leaf))

-- Or in a slightly different representation
--             0
--
--             0
--            / \
--           1   1
--
--             0
--            / \
--           1   1
--          / \ / \
--         2  2 2  2
--
