module Traversable where

-- See
-- http://www.staff.city.ac.uk/~ross/papers/Applicative.pdf

import Control.Monad

-- sequence :: (Monad m, Traversable t) => t (m a) -> m (t a)

x :: IO Int
x = return 10

xs :: [IO Int]
xs = map return [1,2,3]

sxs :: IO [Int]
sxs = sequence xs

-- ap :: Monad m => m (a -> b) -> m a -> m b
-- How can we write sequence using ap

sequence' :: [IO a] -> IO [a]
sequence' [] = return []
sequence' (c : cs) = ap (ap (return (:)) c) (sequence' cs)

data Person = Person {
  name :: String,
  lastName :: String
  } deriving Show

data Address = Address {
  firstLine :: String,
  postCode :: String
} deriving Show

data Err = String

validateName :: String -> Either Err String 
validateName = undefined

validateLastName :: String -> Either Err String 
validateLastName = undefined

main :: IO ()
main = do
  print "hello ahmed, how are you doing mate?"
  return ()