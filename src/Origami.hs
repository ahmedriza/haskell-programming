module Origami where

-- See https://www.cs.ox.ac.uk/jeremy.gibbons/publications/origami.pdf

data List a = Nil | Cons a (List a) deriving (Eq, Ord, Show)

wrap :: a -> List a
wrap x = Cons x Nil

nil :: List a -> Bool
nil Nil         = True
nil (Cons x xs) = False

-- Folds for Lists

-- equivalent to Haskell's foldr
foldL :: (a -> b -> b) -> b -> List a -> b
foldL f e Nil = e
foldL f e (Cons x xs) = f x (foldL f e xs)


