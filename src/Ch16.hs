-- {-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}

module Ch16 where

import Test.QuickCheck
import Test.QuickCheck.Function

---------------------------
-- Chapter 16
-- Functor
---------------------------
type E e = Either e

--
-- In GHCi, :set -XTypeApplications
-- :type fmap @Maybe
-- :type fmap @(Either _)
--
class (Show a) =>
      OneDigit a where
  hello :: a -> String
  hello = show

instance OneDigit Int

instance OneDigit Double

class Something a where
  s :: a -> a

data MyEither a
  = MyNothing
  | MyJust a deriving (Eq, Show)

instance Functor MyEither where
  fmap f MyNothing  = MyNothing
  fmap f (MyJust a) = MyJust (f a)

data FixMePls a
  = FixMe
  | Pls a
  deriving (Eq, Show)

instance Functor FixMePls where
  fmap _ FixMe   = FixMe
  fmap f (Pls a) = Pls (f a)

-------------------

data CountingBad a = Heisenberg Int a deriving (Eq, Show)

-- NOT OK Functor
instance Functor CountingBad where
  fmap f (Heisenberg n a) = Heisenberg (n+1) (f a)

-- Example
hx = Heisenberg 20 100
f' = (+1)
g' = (+2)

-- The composition law is broken here
comp = fmap (f' . g') hx == (fmap f' . fmap g') hx -- False

-- How do we fix this? Easy. Stop changing anything that's not an argument to f

data CountingGood a = Heisenberg' Int a deriving (Eq, Show)

instance Functor CountingGood where
  fmap f (Heisenberg' n a) = Heisenberg' (n) (f a)
hx' = Heisenberg' 20 100
comp' = fmap (f' . g') hx' == (fmap f' . fmap g') hx' -- True

--------------- Commonly Used Functors ---------------------------------------

replaceWithP :: b -> Char
replaceWithP = const 'p'

-- data Mabye a = Nothing | Just a
m1 = fmap replaceWithP (Just 10) -- (Just 'p')
m2 = fmap replaceWithP Nothing -- Nothing

-- data [] a = [] | a : [a]
a1 = fmap replaceWithP [1,2,3] -- "ppp"
a2 = fmap replaceWithP [] -- ""

-- data (,) a b = (,) a b
p1 = fmap replaceWithP (10, 20) -- (10, 'p')
p2 = fmap replaceWithP (10, "woo") -- (10, 'p')

tossEmOne :: Integer -> Integer
tossEmOne = fmap (+1) negate

t1 = tossEmOne 10 -- -9
t2 = tossEmOne (-10) -- 11

------------

n = Nothing
w = Just "woohoo"
ave = Just "Ave"

lms :: [Maybe String]
lms = [ave, n, w]

lms' :: [Char]
lms' = fmap replaceWithP lms --- "ppp"

lms'' :: [Maybe Char]
lms'' = (fmap . fmap) replaceWithP lms --- [Just 'p',Nothing,Just 'p']

lms''' :: [Maybe String]
lms''' = (fmap . fmap . fmap) replaceWithP lms -- [Just "ppp",Nothing,Just "pppppp"]

-- How does (fmap. fmap) work: 
--
-- (1) function composition works like this for ordinary functions
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- (2) fmap
-- fmap :: Functor f => (m -> n) -> f m -> f n
-- fmap :: Functor g => (x -> y) -> g x -> g y
-- (3)
-- (.) fmap fmap = ?
-- (.) :: (Functor f, Functor g) => (a -> b) -> g (f a) -> g (f b)
--

-----------------

ha :: Maybe [String]
ha = Just ["Ha", "Ha"]

lmls :: [Maybe [String]]
lmls = [ha, Nothing, Just []]

lmls' = (fmap . fmap) replaceWithP lmls -- [Just 'p', Nothing, Just 'p']

tripleFmap = (fmap . fmap . fmap) replaceWithP lmls --- [Just "pp", Nothing, Just ""]

quadrupleFmap = (fmap . fmap . fmap . fmap) replaceWithP lmls -- [Just ["pp", "pp"], Nothing, Just []]

----------------

-- Just making the arguments more specific
replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

-- What happens if we lift it?

liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

-- But we can give a more specific type to liftedReplace
liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

-- What if we lifted twice?
twiceLifted :: (Functor f1, Functor f) => f (f1 a) -> f (f1 Char)
twiceLifted = (fmap . fmap) replaceWithP

-- Making it more specific
twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = twiceLifted

thriceLifted :: (Functor f2, Functor f1, Functor f) => f (f1 (f2 a)) -> f (f1 (f2 Char))
thriceLifted = (fmap . fmap . fmap) replaceWithP

-- Making it more specific
thriceLifted' :: [Maybe [Char]] -> [Maybe [Char]]
thriceLifted' = thriceLifted

-- Now we can print the results from our expressions and compare them

main :: IO ()
main = do
  putStr "replaceWithP' lms:  "
  print (replaceWithP' lms)

  putStr "lifedReplace lms:   "
  print (liftedReplace lms)

  putStr "lifedReplace' lms:  "
  print (liftedReplace' lms)

  putStr "twiceLifted lms:    "
  print (twiceLifted lms)

  putStr "twiceLifted' lms:   "
  print (twiceLifted' lms)

  putStr "thriceLifted lms:   "
  print (thriceLifted lms)

  putStr "thriceLifted' lms:  "
  print (thriceLifted' lms)

  print "done"

-------------------------------------------------------------------------------------------
-- Exercises
-------------------------------------------------------------------------------------------
--
-- Add fmap, parentheses, and function composition to the expression as needed for the expression
-- to typecheck and produce the expected result.

-- (1)
-- Expected result [2]
-- a = (+1) $ read "[1]" :: [Int]
a' = fmap (+1) $ read "[1]" :: [Int]

-- (2)
-- b = (++ "lol") (Just ["Hi,", "Hello"])
-- expected result: Just ["Hi,lol", "Hellolol"]
b' = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

-- (3)
-- c = (*2) (\x -> x - 2)
-- expected output: c 1 = -2
c' :: Integer -> Integer
c' = fmap (*2) (\x -> x - 2)

-- (4)
-- d = ((return '1' ++) . show) (\x -> [x, 1..3])
-- expected output: d 0 = "1[0,1,2,3]"
d' = fmap ((return '1' ++) . show) (\x -> [x, 1..3])

-- (5)
-- e :: IO Integer
-- e = let ioi = readIO "1" :: IO Integer
--         changed = read ("123"++) show ioi
--     in (*3) changed
--
-- expected output: e = 3693

-- ioi = readIO "1" :: IO Integer
-- fmap show ioi = "1" :: IO String
-- x = (fmap ("123" ++) (fmap show ioi)) -- = "1231" :: IO String
-- y = fmap read x :: IO Integer
-- z = fmap (*3) y 

e' :: IO Integer
e' = let ioi = readIO "1" :: IO Integer
         changed :: IO Integer
         changed = fmap read (fmap ("123" ++) (fmap show ioi))
    in fmap (*3) changed
          
e'' :: IO Integer
e'' = let ioi = readIO "1" :: IO Integer
          changed = read <$> (("123" ++) <$> (show <$> ioi))
      in (*3) <$> changed

-- =============================================================================
-- Transforming the Unapplied Type Argument
-- =============================================================================

data Two a b = Two a b deriving (Eq, Show)  -- like (,)

data Or a b = First a | Second b deriving (Eq, Show) -- like Either

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Functor (Or a) where
  fmap f (First a) = First a
  fmap f (Second b) = Second (f b)

-- =============================================================================
-- QuickChecking Functor Instances
-- =============================================================================

-- Functor laws
-- (1) fmap id = id
-- (2) fmap (f . g) = (fmap f) . (fmap g)

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Functor f, Eq (f c)) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

fid :: [Int] -> Bool
fid x = functorIdentity x

fc1 :: Maybe Int -> Bool
fc1 = functorCompose (+1) (+2)

li1 :: Maybe Int -> Bool
li1 x = fc1 (x :: Maybe Int)

fc2 :: [Int] -> Bool
fc2 = functorCompose (+1) (+2)

li2 :: [Int] -> Bool
li2 x = fc2 (x :: [Int])
  
test1 :: IO ()
test1 = do
  quickCheck (functorIdentity :: [Int] -> Bool)
  quickCheck fid
  quickCheck li1
  quickCheck li2

-- =============================================================================
-- Making QuickCheck Generate Functions
-- =============================================================================

functorCompose' :: (Functor f, Eq (f c)) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool

test2 :: IO ()
test2 = do
  quickCheck (functorCompose' :: IntFC)
  
-- =============================================================================
-- Exercises: Instances of Func
-- =============================================================================

-- Implement Functor instances for the following datatypes. 

----- (1) ------
newtype Identity a = Identity a deriving (Eq, Show)
instance Functor (Identity) where
  fmap f (Identity a) = Identity (f a)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  x <- arbitrary
  return (Identity x)
  
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen

type IdentityFC = (Identity Int) -> IntToInt -> IntToInt -> Bool

----- (2) ------

data Pair a = Pair a a deriving (Eq, Show)

instance Functor (Pair) where
  fmap f (Pair a b) = Pair (f a) (f b)

pairGen :: Arbitrary a => Gen (Pair a)
pairGen = do
  x <- arbitrary
  return (Pair x x)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = pairGen

type PairFC = (Pair Int) -> IntToInt -> IntToInt -> Bool  

----- (3) ------

twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
twoGen = do
  x <- arbitrary
  y <- arbitrary
  return (Two x y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = twoGen

type TwoFC = (Two Int Int) -> IntToInt -> IntToInt -> Bool  

----- (4) ------

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

threeGen :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
threeGen = do
  x <- arbitrary
  y <- arbitrary
  z <- arbitrary
  return (Three x y z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = threeGen

type ThreeFC = (Three Int Int Int) -> IntToInt -> IntToInt -> Bool  

----- (5) ------

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

threeGen' :: (Arbitrary a, Arbitrary b) => Gen (Three' a b)
threeGen' = do
  x <- arbitrary
  y <- arbitrary
  return (Three' x y y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = threeGen'
  
type ThreeFC' = (Three' Int Int) -> IntToInt -> IntToInt -> Bool  

----- (6) ------

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

fourGen :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Gen (Four a b c d)
fourGen = do
  s <- arbitrary
  t <- arbitrary
  u <- arbitrary
  v <- arbitrary
  return (Four s t u v)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = fourGen

type FourFC = (Four Int Int Int Int) -> IntToInt -> IntToInt -> Bool  

----- (7) ------

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

fourGen' :: (Arbitrary a, Arbitrary b) => Gen (Four' a b)
fourGen' = do
  s <- arbitrary
  t <- arbitrary
  return (Four' s s s t)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = fourGen'

type FourFC' = (Four' Int Int) -> IntToInt -> IntToInt -> Bool  

--------------

test3 :: IO ()
test3 = do
  quickCheck (functorIdentity :: (Identity Int) -> Bool)
  quickCheck (functorCompose' :: IdentityFC)

  quickCheck (functorIdentity :: (Pair Int) -> Bool)
  quickCheck (functorCompose' :: PairFC)

  quickCheck (functorIdentity :: (Two Int Int) -> Bool)
  quickCheck (functorCompose' :: TwoFC)

  quickCheck (functorIdentity :: (Three Int Int Int) -> Bool)
  quickCheck (functorCompose' :: ThreeFC)

  quickCheck (functorIdentity :: (Three' Int Int) -> Bool)
  quickCheck (functorCompose' :: ThreeFC')

  quickCheck (functorIdentity :: (Four Int Int Int Int) -> Bool)
  quickCheck (functorCompose' :: FourFC)

  quickCheck (functorIdentity :: (Four' Int Int) -> Bool)
  quickCheck (functorCompose' :: FourFC')

-- =============================================================================
-- Ignoring Possibilities
-- =============================================================================


