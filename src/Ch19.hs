{-# LANGUAGE OverloadedStrings #-}

module Ch19 where

import Control.Monad (join, (>=>), liftM2)

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Web.Scotty
import Data.Monoid (mconcat)

import qualified Data.Map as M

--
-- Chapter 19
--

scottyTest :: IO ()
scottyTest = scotty 3000 $
  (get "/" $ do
      html "hello there"
  )
  >>
  (get "/hello" $ do
      name <- param "name"
      html $ mconcat ["<h1>Hello ", name, "!</h1>"]
  )
  >>
  (get "/:word" $ do
      beam <- param "word"
      html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
  )
  >>
  (notFound $ do
      text "Not Found"
  )


fm = M.fromList [('a', 1)]
gm = M.fromList [('b', 2)]


