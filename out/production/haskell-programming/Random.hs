module Random where

import Test.QuickCheck
import Test.QuickCheck.Gen
import System.Random

gen = mkStdGen 1

-- Generate sequence of random numbers starting with the geneator, gen

(r1, g1) = random gen :: (Int, StdGen)
(r2, g2) = random g1 :: (Int, StdGen)
(r3, g3) = random g2 :: (Int, StdGen)

main :: IO ()
main = do
  return ()
