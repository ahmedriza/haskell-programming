{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Ch20 where

import Data.Maybe (fromMaybe)
import Data.Foldable
import Data.Monoid

import Debug.Trace

--
-- Chapter 20: Foldable
--

class MyFoldable (t :: * -> *) where
  fold_ :: Monoid m => t m -> m
  foldMap_ :: Monoid m => (a -> m) -> t a -> m

sum1 = foldr (+) 0 [1..10]

sum2 :: Sum Int
sum2 = fold [Sum 10, Sum 30]

-- xs = map Sum [1..10]
xs :: [Sum Integer]
xs = [Sum 1, Sum 2, Sum 3, Sum 4, Sum 5]

sum_xs = fold xs

strs = ["hello ", "ahmed"]
sum_strs = fold strs

sum3 = foldMap Sum [1..10]

sum4 = foldMap All [False, True, True]

--

data MyFirst a = MyFirst { getMyFirst :: Maybe a } deriving (Eq, Show)
data MyLast a = MyLast { getMyLast :: Maybe a } deriving (Eq, Show)

instance Show a => Monoid (MyFirst a) where
  mempty = MyFirst Nothing
  MyFirst Nothing `mappend` r = (trace ("r = " <> show r) r)
  l1 `mappend` r1 = (trace ("l1 = " <> show l1 <> ", r1 = " <> show r1) l1)

instance Show a => Monoid (MyLast a) where
  mempty = MyLast Nothing
  l `mappend` MyLast Nothing = trace ("l = " <> show l) l
  l1 `mappend` r1 = trace ("l1 = " <> show l1 <> ", r1 = " <> show r1) r1

maybes :: [Maybe Integer]
maybes = [Nothing, Nothing, Just 10, Just 20, Nothing, Just 5, Nothing]

-- foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
-- foldMap f = foldr (mappend . f) mempty
--
-- > foldr f z [x1, x2, ..., xn] == x1 `f` (x2 `f` ... (xn `f` z)...)

sum5 = foldMap MyFirst maybes

sum6 = foldMap MyLast maybes

sumXs = map Sum [2..4]

--------------------------------------------------------------------------------
-- 20.4 Demonstrating Foldable Instances
--------------------------------------------------------------------------------

data Identity a = Identity a deriving Show

instance Foldable Identity where
  foldr f z (Identity x) = f x z

  foldl f z (Identity x) = f z x

  foldMap f (Identity x) = f x

fm :: (Foldable t, Monoid m, Num m) => t m -> m
fm = foldMap (*5)

type PI = Product Integer

fm1 = fm (Identity 100) :: PI


-- Maybe

fm2 = foldMap (*5) (Just 10) :: Sum Integer -- == Sum { getSum = 50 }

data Optional a = Nada | Yep a

instance Foldable Optional where
  foldr f z Nada = z
  foldr f z (Yep x) = f x z

  foldl f z Nada = z
  foldl f z (Yep x) = f z x

  foldMap f Nada = mempty
  foldMap f (Yep x) = f x
  
--------------------------------------------------------------------------------
-- 20.5 Some Basic Derived Operations
--------------------------------------------------------------------------------

-- toList :: t a -> [a]

list1 = toList (Just 10) -- [10]
list2 = toList (1, 2) -- [2]
list3 = toList (Yep 100) -- [100]

jxs :: [Maybe Integer]
jxs = [Just 1, Just 2, Just 3]

jxs_list :: [[Integer]]
jxs_list = map toList jxs -- [[1], [2], [3]]

-- concatMap
list4 = concatMap (\x -> [fmap (*10) x]) jxs -- [Just 10, Just 20, Just 30]

-- null, test whether the structure is empty
-- null :: t a -> Bool
-- null = foldr (\_ _ -> False) True

n1 = null Nada -- True
n2 = null (Yep 20) -- False
n3 = null (Left 10) -- True
n4 = null (Right "hello") -- False

-- length: returns the size/length of a finite structure as an 'Int'
-- length :: t a -> Int

tuples = [(1,2), (3,4), (5,6)]
tuples_len = fmap length tuples -- [1, 1, 1]

len2 = fmap length Just [10, 20, 30] -- gives 1, why?
len3 = length $ Just [10, 20, 30] -- 1

-- elem: Does the element occur in the data structure
-- elem :: Eq a => a -> t a -> Bool

e1 = elem 20 (Just 20) -- True
e2 = elem 20 Nothing   -- False
e3 = elem 3 (Right 3)  -- True
e4 = elem 3 (Left 3)   -- False

-- | The largest element
--   of a non-empty structure
-- maximum :: Ord a => t a -> a

-- | The least element
--   of a non-empty structure
-- minimum :: Ord a => t a -> a

max1 = maximum [10, 100, 20]                  -- 100
max2 = fmap maximum [Just 2, Just 10, Just 4] -- [2, 10, 4]
max3 = fmap maximum Just [3, 7, 10, 2]        -- [3, 7, 10, 2]
max4 = fmap maximum (Just [30, 70, 10, 20])   -- Just 70

-- | The 'sum' function computes the sum of the numbers of a structure.
sum' :: (Foldable t, Num a) => t a -> a
sum' xs = getSum . foldMap Sum $ xs

s1 = fmap sum'        (Just [1,2,3,4])
--   fmap (a -> b) -> f     a          -> f b
--   Hence a = [1,2,3,4]


-- Exercises
-- Library Functions

-- Implement these functions in terms of foldMap or foldr from Foldable, then try
-- them out with multiple types that have Foldable instances.

-- (1) This is nicer with foldMap, but foldr is fine too.

sum'' :: (Foldable t, Num a) => t a -> a
sum'' = foldr (+) 0

product' :: (Foldable t, Num a) => t a -> a
product' = foldr (*) 1

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = getAny . foldMap (Any . (== x)) 

elem'' :: (Eq a, Functor t, Foldable t) => a -> t a -> Bool
elem'' a xs = getAny $ fold $ fmap Any $ fmap (== a) xs

js = Just 10
es = [10, 20, 100, 30, 1]

elemTest1 = elem' 10 es
elemTest2 = elem' 10 js

---

newtype Max a = Max {getMax :: Maybe a} deriving Show
newtype Min a = Min {getMin :: Maybe a} deriving Show

instance Ord a => Monoid (Max a) where
  mempty = Max Nothing
  Max Nothing `mappend` rhs = rhs
  lhs `mappend` Max Nothing = lhs
  Max l@(Just x) `mappend` Max r@(Just y) = case x > y of
    True -> Max l
    otherwise -> Max r

instance Ord a => Monoid (Min a) where
  mempty = Min Nothing
  Min Nothing `mappend` rhs = rhs
  lhs `mappend` Min Nothing = lhs
  Min l@(Just x) `mappend` Min r@(Just y) = case x > y of
    True -> Min r
    otherwise -> Min l

mmax1 = Max Nothing
mmax2 = Max (Just 10)
mmax3 = Max (Just 20)
  
minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = getMin . foldMap (Min . Just) 

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = getMax . foldMap (Max . Just) 

null' :: (Foldable t) => t a -> Bool
null' = foldr (\_ _ -> False) True 

length' :: (Foldable t) => t a -> Int
length' = foldr (\_ y -> y + 1) 0

l1 = length es
l2 = length' es

-- See the following for a discussion of build and its relation
-- to fold.
-- https://github.com/quchen/articles/blob/master/build.md

build :: forall a. (forall b. (a -> b -> b) -> b -> b) -> [a]
build g = g (:) []

toList' :: (Foldable t) => t a -> [a]
-- equivalent to, foldr (:) [], but can be more efficient
toList' xs = build (\c n -> foldr c n xs) 

myLambdaList :: (Integer -> t -> t) -> t -> t
myLambdaList = \cons nil -> 1 `cons` (2 `cons` (3 `cons` nil))

llist1 = myLambdaList (:) [] -- [1,2,3]

tl1 = toList (Just 10)

-- Hint: Use foldMap
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

-- Defined foldMap in terms of foldr
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (mappend . f) mempty

ts = [Sum 1, Sum 2, Sum 3, Sum 4, Sum 5]

---

--------------------------------------------------------------------------------
-- 20.6 Chapter Exercises
--------------------------------------------------------------------------------

-- Write Foldable instances for the following data types

data Constant a b = Constant b

instance Foldable (Constant a) where
  foldMap f (Constant b) = f b

data Two a b = Two a b

instance Foldable (Two a) where
  foldMap f (Two x y) = f y

data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

data Three' a b = Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' x y z) = f y

data Four' a b = Four' a b b b

instance Foldable (Four' a) where
  foldMap f (Four' s t u v) = f t

-- Write a filter function for Foldable types  using foldMap

filterF :: (Applicative f
           , Foldable t
           , Monoid (f a))
        => (a -> Bool) -> t a -> f a
filterF p ts = undefined
