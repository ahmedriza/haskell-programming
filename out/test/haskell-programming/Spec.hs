module Main where

import Morse
import qualified Data.Map as M
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Gen (oneof)

import Data.List (sort)

main :: IO ()
main = do
  -- quickCheck prop_thereAndBackAgain
  -- quickCheck (\x -> halfIdentity x == x)
  -- quickCheck (\x -> listOrdered (sort x :: [Int]))
  -- quickCheck (\x y z -> plusAssociative (x::Int) (y::Int) (z::Int))
  -- quickCheck (\x y -> plusCommutative (x::Int) (y::Int))
  -- quickCheck $ forAll genPos (\x y -> x + y > 0)
  -- quickCheck (quotRemainder :: Positive Int -> Positive Int -> Bool)
  -- quickCheck (forAll (arbitrary :: Gen Double) (\x -> halfIdentity x == x))
  quickCheck (\x -> reverse_property (x :: [Int]) == id x)

testQuick :: IO ()
testQuick = hspec $ do
    describe "QC" $ do
      it "quot rem" $ do
        property $ \x y -> (quot x y)*(y::Int) + (rem x y) == (x:: Int)

---------------------------------------------------------------------------------------------

dollar :: (Int -> Int) -> Int -> Int
dollar f a = f $ a 

---------------------------------------------------------------------------------------------

half x = x / 2

halfIdentity :: Double -> Double
halfIdentity = (*2) . half

---------------------------------------------------------------------------------------------

prop_listOrdered :: [Int] -> Bool
prop_listOrdered xs = listOrdered (sort xs)

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = 
  snd $ foldr go (Nothing, True) xs
  where
    go :: Ord a => a -> (Maybe a, Bool) -> (Maybe a, Bool)
    go _ status@(_, False) = status
    go y (Nothing, t) = (Just y, t)
    go y (Just x, t) = (Just y, x >= y)

---------------------------------------------------------------------------------------------

plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative x y = x + y == y + x

---------------------------------------------------------------------------------------------

genPos :: Gen Int
genPos = abs `fmap` (arbitrary :: Gen Int) `suchThat` (> 0)

quotRemainder :: (Integral a) => (Positive a) -> (Positive a) -> Bool
quotRemainder (Positive x) (Positive y) = (quot x y) * y  + (rem x y) == x

---------------------------------------------------------------------------------------------

reverse_property = reverse . reverse

---------------------------------------------------------------------------------------------

prop_thereAndBackAgain :: Property
prop_thereAndBackAgain =
  forAll charGen
  (\c -> ((charToMorse c) >>= morseToChar) == Just c)
  
---------------------------------------------------------------------------------------------

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


