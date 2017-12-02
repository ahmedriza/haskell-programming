module QuickCheckTest where

import Test.QuickCheck

main :: IO ()
main = do
  runQC
  return ()

runQC :: IO ()
runQC = quickCheck prop_additionGreater

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

mult :: (Eq a, Num a) => a -> a -> a
mult x y = go x y 0
  where
    go a b acc
      | b == 0 = acc
      | otherwise = go a (b-1) (acc+a)
