{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Semigroup
import Test.QuickCheck hiding (Success, Failure)
-- import Test.QuickCheck.Poly
-- import Test.QuickCheck.Function
import Text.Show.Functions

import Generics.Deriving

---------------------------------------------------------
-- Chapter 15 SemiGroup Exercises
---------------------------------------------------------

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  Trivial <> Trivial = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semiGroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semiGroupAssoc a b c = ((a <> b) <> c) == (a <> (b <> c))

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

------------

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  x <- arbitrary
  return (Identity x)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen

type IdentityAssoc a = Identity a -> Identity a -> Identity a -> Bool

------------

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two c d) = Two (a <> c) (b <> d)

twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
twoGen = do
  x <- arbitrary
  y <- arbitrary
  return (Two x y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = twoGen

type TwoAssoc a b = Two a b -> Two a b -> Two a b -> Bool

s = Sum 10
t = Sum 20
u = Sum 30
v = Sum 40

t1 = Two s t
t2 = Two u v

t1t2 = t1 <> t2

------------

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (Four a1 b1 c1 d1) <> (Four a2 b2 c2 d2) = Four (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2)

fourGen :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Gen (Four a b c d)
fourGen = do
  t <- arbitrary
  u <- arbitrary
  v <- arbitrary
  w <- arbitrary
  return (Four t u v w)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = fourGen

type FourAssoc a b c d = Four a b c d -> Four a b c d -> Four a b c d -> Bool  

------------

newtype BoolConj = BoolConj Bool deriving (Eq, Show)
-- what it should do
-- (BoolConj True) <> (BoolConj True) = BoolConj True
-- (BoolConj True) <> (BoolConj False) = BoolConj False

instance Semigroup BoolConj where
  BoolConj _     <> BoolConj False = BoolConj False
  BoolConj False <> BoolConj _     = BoolConj False  
  BoolConj True  <> BoolConj True  = BoolConj True

instance Arbitrary BoolConj where
  arbitrary = frequency [(1, return (BoolConj True)),
                         (1, return (BoolConj False))
                        ]

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

------------

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  BoolDisj False <> BoolDisj False = BoolDisj False
  BoolDisj True  <> BoolDisj _     = BoolDisj True
  BoolDisj _     <> BoolDisj True  = BoolDisj True

instance Arbitrary BoolDisj where
  arbitrary = frequency [(1, return (BoolDisj True)),
                         (1, return (BoolDisj False))
                        ]
              
type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

------------

data Or a b = Fst a | Snd b deriving (Show, Eq)

-- Fst 1 <> Snd 2 = Snd 2
-- Fst 1 <> Fst 2 = Fst 2
-- Snd 1 <> Fst 2 = Snd 1
-- Snd 1 <> Snd 2 = Snd 1

instance Semigroup (Or a b) where
  Fst _ <> Snd b = Snd b
  Fst _ <> Fst a = Fst a
  Snd a <> Fst _ = Snd a
  Snd a <> Snd b = Snd b

fstGen :: Arbitrary a => Gen (Or a b)
fstGen = do
  x <- arbitrary
  return (Fst x)

sndGen :: Arbitrary b => Gen (Or a b)
sndGen = do
  x <- arbitrary
  return (Snd x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = frequency [(1, fstGen), (2, sndGen)]

type OrAssoc a b = Or a b -> Or a b -> Or a b -> Bool

------------

newtype Combine a b = Combine { unCombine :: a -> b } deriving Generic

f = Combine $ \n -> Sum (n + 1)
g = Combine $ \n -> Sum (n - 1)
fx = unCombine f $ 10
gx = unCombine g $ 10

-- unCombine (f <> g) $ 0 = Sum {getSum = 0}
-- unCombine (f <> g) $ 1 = Sum {getSum = 2}
-- unCombine (f <> f) $ 1 = Sum {getSum = 4}
-- unCombine (g <> f) $ 1 = Sum {getSum = 2}

-- Hint: This function will eventually be applied to a single value of type a. But you'll have
-- multiple functions that can produce a value of type b. How do we combine multiple values so
-- we have a single b? Remember the type of the value inside Combine is that of a 'function'.
-- The type of functions should already have an Aribitrary instance that you can reuse for testing
-- this instance.
 
instance Semigroup b => Semigroup (Combine a b) where
  (Combine f') <> (Combine g') = Combine (f' <> g')

-- See the definition of instance (a -> b) in Data.Semigroup to see why we need the
-- constraint (Semigroup b) in the above instance.
-- from Data.Semigroup:
-- instance Semigroup b => Semigroup (a -> b) where
--  f <> g = \a -> f a <> g a

combineGen :: (CoArbitrary a, Arbitrary b) => Gen (Combine a b)
combineGen = do
  f <- arbitrary
  return (Combine f)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = combineGen

type CombineAssoc a b = Combine a b -> Combine a b -> Combine a b -> Bool  

instance Show (Combine a b) where
  show (Combine f) = "Combine: " <> (show f)
  
prop_fun :: (Int -> Int) -> Int -> Bool
prop_fun f x = (f . (+2))x  == (f . (*2)) x

gen :: Gen Int
gen = (variant 100 (arbitrary :: Gen Int))

instance (Arbitrary a, CoArbitrary b) => CoArbitrary (Combine a b)
                     
instance (Num a, Eq b) => Eq (Combine a b) where
  (==) (Combine f) (Combine g) = f x == g x
    where x = (-1)

-- This needs Test.QuickCheck.Poly
-- prop_MapFilter :: (Int -> Int) -> (Int -> Bool) -> [Int] -> Bool
-- prop_MapFilter f p (xs :: [A]) =
--  map f (filter p xs) == filter p (map f xs)
  
------------

data Validation a b = Failure a | Success b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (<>) (Success s) (Failure f) = Success s
  (<>) (Success s1) (Success s2) = Success s1
  (<>) (Failure f) (Success s) = Success s
  (<>) (Failure f1) (Failure f2) = Failure (f1 <> f2)

testValidation :: IO ()
testValidation = do
  let failure :: String -> Validation String Int
      failure = Failure
      success :: Int -> Validation String Int
      success = Success

  print $ success 1 <> failure "blah"      -- success 1
  print $ failure "woot" <> failure "blah" -- Failure "wootblah"
  print $ success 1 <> success 2           -- success 1
  print $ failure "woot" <> success 2      -- success 2
  return ()

------------

main :: IO ()
main = do
  quickCheck (semiGroupAssoc :: TrivialAssoc) -- associative law is OK
  quickCheck (semiGroupAssoc :: IdentityAssoc (Sum Int))
  quickCheck (semiGroupAssoc :: IdentityAssoc (Any))
  quickCheck (semiGroupAssoc :: TwoAssoc (Sum Int) (Sum Int))
  quickCheck (semiGroupAssoc :: TwoAssoc Any Any)
  quickCheck (semiGroupAssoc :: FourAssoc Any Any Any Any)
  quickCheck (semiGroupAssoc :: FourAssoc (Sum Int) (Sum Int) (Sum Int) Any)
  quickCheck (semiGroupAssoc :: BoolConjAssoc)
  quickCheck (semiGroupAssoc :: BoolDisjAssoc)
  quickCheck (semiGroupAssoc :: OrAssoc Int Int)
  quickCheck (semiGroupAssoc :: OrAssoc Float Float)
  quickCheck (semiGroupAssoc :: CombineAssoc Int (Sum Int))
  -- quickCheck prop_fun
  -- quickCheck prop_MapFilter
