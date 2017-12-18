module Random where

import System.Random
import Test.QuickCheck
import Test.QuickCheck.Gen

gen = mkStdGen 1

(r1, g1) = random gen :: (Int, StdGen)

(r2, g2) = random g1 :: (Int, StdGen)

(r3, g3) = random g2 :: (Int, StdGen)

-- dice :: Gen Int
-- dice = choose (1, 6)
main :: IO ()
main = do
  let (a, gen') = random gen :: (Int, StdGen)
  let (b, gen'') = random gen' :: (Int, StdGen)
  print a
  print b
  print "done"
