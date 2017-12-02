module Main where

import Morse
import qualified Data.Map as M
import Test.QuickCheck
import Test.QuickCheck.Gen (oneof)

main :: IO ()
main = quickCheck prop_thereAndBackAgain

prop_thereAndBackAgain :: Property
prop_thereAndBackAgain =
  forAll charGen
  (\c -> ((charToMorse c) >>= morseToChar) == Just c)
  
---

allowedChars :: [Char]
allowedChars = M.keys letterToMorse

allowedMorse :: [Morse]
allowedMorse = M.elems letterToMorse

charGen :: Gen Char
charGen = elements allowedChars

morseGen :: Gen Morse
morseGen = elements allowedMorse

---

data Trivial = Trivial deriving (Eq, Show)

trivialGen :: Gen Trivial
trivialGen = return Trivial

instance Arbitrary Trivial where
  arbitrary = trivialGen

data Identity a = Identity a deriving (Eq, Show)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  a <- arbitrary
  return (Identity a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen

identityGenInt :: Gen (Identity Int)
identityGenInt = identityGen

data Pair a b = Pair a b deriving (Eq, Show)

pairGen :: (Arbitrary a, Arbitrary b) => Gen (Pair a b)
pairGen = do
  a <- arbitrary
  b <- arbitrary
  return (Pair a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = pairGen

  
-- Sum type

data Sum a b = First a
             | Second b
             deriving (Eq, Show)

sumGenEqualProbability :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenEqualProbability = do
  a <- arbitrary
  b <- arbitrary
  oneof [return $ First a, return $ Second b]

sumGenCharInt :: Gen (Sum Char Int)
sumGenCharInt = sumGenEqualProbability

sumGenBiasedProbability :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenBiasedProbability = do
  a <- arbitrary
  b <- arbitrary
  frequency [(10, return $ First a), (1, return $ First b)]

sumGenIntInt :: Gen (Sum Int Int)
sumGenIntInt = sumGenBiasedProbability
