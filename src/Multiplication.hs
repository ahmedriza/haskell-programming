module Multiplication where

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Multiplication" $ do
    it "15 multiplied by 0 is 0" $ do
      mult 15 0 `shouldBe` 0
    it "15 multiplied by 1 is 15" $ do
      mult 15 1 `shouldBe` 15
    it "15 multiplied by 3 is 45" $ do
      mult 15 3 `shouldBe` 45
    it "22 multiplied by 5 is 110" $ do
      mult 22 5 `shouldBe` 110

    it "1 + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)


mult :: (Eq a, Num a) => a -> a -> a
mult x y = go x y 0
  where
    go a b acc
      | b == 0 = acc
      | otherwise = go a (b-1) (acc+a)

trivialGen :: Gen Int
trivialGen = return 1

oneThroughThree :: Gen Int
oneThroughThree = elements [1,2,3]

genBool :: Gen Bool
genBool = choose (True, False)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

tuples = sample' (genTuple :: Gen(Float, Float))

tuples' = sample' (genTuple :: Gen([Int],Int))

genTriple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genTriple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

triples = sample' (genTriple :: Gen(Int, Char, Float))

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

eithers = sample' (genEither :: Gen (Either Int Int))

genMaybe :: (Arbitrary a) => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]

maybes = sample' (genMaybe :: Gen (Maybe Int))

genMaybe' :: (Arbitrary a) => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [(1, return Nothing), (3, return (Just a))]

maybes' = sample' (genMaybe' :: Gen (Maybe Int))

--------------------------------------
-- f (arbitrary :: Gen (Maybe Double))
--------------------------------------
f :: (Show a) => Gen a -> IO ()
f gen = do
  s <- sample' gen
  print s
  return ()
