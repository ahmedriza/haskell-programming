module Addition where

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "15 divide by 3 is 5" $ do
      divideBy 15 3 `shouldBe` (5, 0)
    it "22 divide by 5 is\
       \ 4 remainder 2" $ do
      divideBy 22 5 `shouldBe` (4, 2)


divideBy :: Integral a => a -> a -> (a, a)
divideBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)

mult :: (Eq a, Num a) => a -> a -> a
mult x y = go x y 0
  where
    go a b acc
      | b == 0 = acc
      | otherwise = go a (b-1) (acc+a)
