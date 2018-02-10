{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AesonTest where

import Data.List

import qualified Data.ByteString.Lazy as BS
import Data.Aeson
import Data.Aeson.Types
import GHC.Exts
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as E

import qualified Data.HashMap.Strict as HM

-- JSON parsing

readFile' :: String -> IO (BS.ByteString)
readFile' filename = do
  file <- BS.readFile filename
  return file

myFunction :: forall a . Ord a => [a] -> [(a, a)]
myFunction inputList = zip sortedList nubbedList
  where
    sortedList :: [a]
    sortedList = sort inputList
    nubbedList :: [a]
    nubbedList = nub inputList

val :: Value
val = Object $ fromList [
  ("numbers", Array $ fromList [Number 1, Number 2, Number 3]),
  ("boolean", Bool True)]

valText = T.putStrLn . E.decodeUtf8 . encode $ val

data Coord = Coord { x :: Double, y :: Double, z :: Double } deriving Show

instance FromJSON Coord where
  parseJSON (Object v) = Coord
    <$> v .: "x"
    <*> v .: "y"
    <*> v .: "z"
  parseJSON invalid = typeMismatch "Coord" invalid

coord :: Maybe Coord = decode "{\"x\" : 100, \"y\" : 200, \"z\" : 300}"

pairs' :: IO [(String, Integer)]
pairs' = return [("a", 40)]
