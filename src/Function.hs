{-# LANGUAGE TypeOperators, GADTs #-}

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Poly

import Data.Char
import Data.Word
import Data.List( intersperse )
import Data.Maybe( fromJust )

data a :-> c where
  Pair  :: (a :-> (b :-> c)) -> ((a,b) :-> c)
  (:+:) :: (a :-> c) -> (b :-> c) -> (Either a b :-> c)
  Unit  :: c -> (() :-> c)
  Nil   :: a :-> c
  Table :: Eq a => [(a,c)] -> (a :-> c)
  Map   :: (a -> b) -> (b -> a) -> (b :-> c) -> (a :-> c)

instance Functor ((:->) a) where
  fmap f (Pair p)    = Pair (fmap (fmap f) p)
  fmap f (p:+:q)     = fmap f p :+: fmap f q
  fmap f (Unit c)    = Unit (f c)
  fmap f Nil         = Nil
  fmap f (Table xys) = Table [ (x,f y) | (x,y) <- xys ]
  fmap f (Map g h p) = Map g h (fmap f p)

instance (Show a, Show b) => Show (a:->b) where
  show p = showFunction p Nothing

-- only use this on finite functions
showFunction :: (Show a, Show b) => (a :-> b) -> Maybe b -> String
showFunction p md =
  "{" ++ concat (intersperse ", " ( [ show x ++ "->" ++ show c
                                    | (x,c) <- table p
                                    ]
                                 ++ [ "_->" ++ show d
                                    | Just d <- [md]
                                    ] )) ++ "}"

-- turning a concrete function into an abstract function (with a default result)
abstract :: (a :-> c) -> c -> (a -> c)
abstract (Pair p)    d (x,y) = abstract (fmap (\q -> abstract q d y) p) d x
abstract (p :+: q)   d exy   = either (abstract p d) (abstract q d) exy
abstract (Unit c)    _ _     = c
abstract Nil         d _     = d
abstract (Table xys) d x     = head ([y | (x',y) <- xys, x == x'] ++ [d])
abstract (Map g _ p) d x     = abstract p d (g x)

-- generating a table from a concrete function
table :: (a :-> c) -> [(a,c)]
table (Pair p)    = [ ((x,y),c) | (x,q) <- table p, (y,c) <- table q ]
table (p :+: q)   = [ (Left x, c) | (x,c) <- table p ]
                 ++ [ (Right y,c) | (y,c) <- table q ]
table (Unit c)    = [ ((), c) ]
table Nil         = []
table (Table xys) = xys
table (Map _ h p) = [ (h x, c) | (x,c) <- table p ]

