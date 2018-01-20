{-# LANGUAGE OverloadedStrings #-}

module Ch19 where

import Control.Monad (join, (>=>), liftM2)

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

--
-- Chapter 19
--

