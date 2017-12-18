{-# LANGUAGE DataKinds #-}

module Ch16 where

import Test.QuickCheck

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
  | MyJust a

instance Functor MyEither where
  fmap f MyNothing = MyNothing
  fmap f (MyJust a) = MyJust (f a)
