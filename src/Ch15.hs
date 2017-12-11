--
-- Chapter 15: Monoid and Semigroup
--

import Data.Monoid
import Test.QuickCheck
import Control.Monad

-- <>: mappend

ff = (Sum 10) <> (Sum 20)

-- newtype All and Any defined in Data.Monoid are the monoids of Bool
all_1 = (All True) <> (All False) == (All False)
all_2 = (All True) <> (All True) == (All True)

any_1 = (Any True) <> (Any False) == (Any True)
any_2 = (Any False) <> (Any False) == (Any False)

-- Maybe
m1 = First (Just 10) <> First (Just 20)
m2 = Last (Just 10) <> Last (Just 20)
m3 = First Nothing <> First Nothing <> First (Just 20)
m4 = Last Nothing <> Last Nothing <> Last (Just 50) <> Last Nothing
m5 = First Nothing <> First Nothing

-- Expected output
-- Only (Sum 1) <> Only (Sum 1) = Only (Sum {getSum = 2})
--
-- Only (Product 4) <> Only (Product 2) = Only (Product {getProduct = 8})
--
-- Only (Sum 1) <> Nada = Only (Sum {getSum = 1})
--
-- Only [1] <> Nada = Only [1]
--
-- Nada <> Only (Sum 1) = Only (Sum {getSum = 1})

-- Mad Lib

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbin' e adv noun adj =
  e <> "! he said " <>
  adv <> " as he jumped into his car " <>
  noun <> " and drove off with his " <>
  adj <> " wife."

-- Rewrite it using mconcat
madlibbinBetter' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbinBetter' e adv noun adj = mconcat
  [e, "! he said ", adv, " as he jumped into his car ", noun, " and drove off with his ", adj, " wife"]

f1 = \a b c -> a + (b + c) == (a + b) + c
f2 = \a b c -> a * (b * c) == (a * b) * c

assoc :: (Eq a) => (a -> a -> a) -> a -> a -> a -> Bool
assoc = \f a b c -> f a (f b c) == f (f a b) c

assoc' :: (Eq a) => (a -> a -> a) -> a -> a -> a -> Bool
assoc' = \ (<>) a b c -> a <> (b <> c) == (a <> b) <> c


---
--- Test associativity
---

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

data Bull = Fools
          | Twoo
          deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = frequency [(1, return Fools), (1, return Twoo)]

instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools

type BullMappend = Bull -> Bull -> Bull -> Bool  

-----------------------------------------------------------------------------------------

-- Write the Monoid instance for our own Maybe type renamed to Optional

data Optional a = Nada
                | Only a
                deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mappend Nada (Only a) = Only a
  mappend (Only a) Nada = Only a
  mappend (Only a) (Only b) = Only (mappend a b)
  mempty = Nada

-----------------------------------------------------------------------------------------

-- Exercise: Maybe Another Monoid
--
-- Write a Monoid instance for a Maybe type which doesn’t require a Monoid
-- for the contents. Reuse the Monoid law QuickCheck properties and use
-- them to validate the instance.

-- data MyPair a b = MyPair { first :: a, second :: b } deriving (Eq, Show)

newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)

genOnly :: Arbitrary a => Gen (Optional a)
genOnly = do
  x <- arbitrary
  return (Only x)

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = frequency [(1, return Nada), (1, genOnly)]

genFirst :: Arbitrary a => Gen (First' a)
genFirst = do
  x <- arbitrary
  return (First' x)

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = genFirst

instance Monoid (First' a) where
  mempty = First' Nada
  mappend (First' Nada) (First' (Only a)) = First' (Only a)
  mappend (First' Nada) (First' Nada) = First' Nada
  mappend (First' (Only a)) (First' Nada)  = First' (Only a)
  mappend (First' (Only a)) (First' (Only b)) = First' (Only a)

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool

f'1 = First' Nada
f'2 = First' (Only 10)

-----------------------------------------------------------------------------------------

main :: IO()
main = do
  let ma = monoidAssoc
      mli = monoidLeftIdentity
      mri = monoidRightIdentity

  -- quickCheck (ma :: BullMappend)   -- associative law is OK
  -- quickCheck (mli :: Bull -> Bool) -- not OK
  -- quickCheck (mri :: Bull -> Bool) -- not OK

  quickCheck (ma :: FirstMappend) -- associative law is OK
  quickCheck (mli :: FstId)       -- OK
  quickCheck (mri :: FstId)       -- OK
  
  -- verboseCheck (monoidAssoc :: String -> String -> String -> Bool)
  -- quickCheck (monoidAssoc :: String -> String -> String -> Bool)
  -- quickCheck (monoidLeftIdentity :: String -> Bool)
  -- quickCheck (monoidRightIdentity :: String -> Bool)
  
  
