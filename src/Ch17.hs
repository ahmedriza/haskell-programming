{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

module Ch17 where
--
-- Chapter 17: Applicative
--

import Data.Monoid
import Control.Applicative
import Data.Monoid
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

--- Note how the 1st Monoid instance of these tuples get combined

p1 = ("Woo", (+1)) <*> (" Hoo!", 1) -- == ("Woo Hoo!", 2)

p2 = (Sum 2, (+1)) <*> (Sum 1, 10)  -- == (Sum 3, 11)

p3 = (Product 3, (+9)) <*> (Product 2, 8) -- == (Product 6, 17)

p4 = (All True, (+1)) <*> (All False, 1)  -- == (All False, 2)

-- It doesn't really matter what Monoid, but we need some way of combining or chosing our values.


-- ===========================================================
-- Tuple Monoid and Applicative Side by Side
-- ===========================================================

-- Here's how the Appicative instance is defined for a Tuple

-- We're defining our own Tuple for illustration since Applicative instance for (,) already exists.
data Tuple a b = Tuple a b deriving (Eq, Show)

-- We have to implement both the Monoid and Functor instances before we can do Applicative

instance (Monoid a, Monoid b) => Monoid (Tuple a b) where
  mempty = Tuple mempty mempty
  mappend (Tuple a b) (Tuple a' b') = Tuple (mappend a a') (mappend b b')

instance Functor (Tuple a) where
  fmap f (Tuple a b) = Tuple a (f b)
                                                           
instance Monoid a => Applicative (Tuple a) where
  pure x = Tuple mempty x
  (<*>) (Tuple a f) (Tuple a' b') = Tuple (mappend a a') (f b')

t1 = Tuple "riza" 47
t2 = Tuple "ahmed " (+1)

t3 = t2 <*> t1 -- == Tuple "ahmed riza" 48

-- ===========================================================
-- Maybe Monoid and Applicative
-- ===========================================================

data MyMaybe a = MyNothing | MyJust a deriving (Eq, Show)

instance Monoid a => Monoid (MyMaybe a) where
  mempty = MyNothing
  mappend m MyNothing = m
  mappend MyNothing m = m
  mappend (MyJust a) (MyJust a') = MyJust (mappend a a')

instance Functor (MyMaybe) where
  fmap _ MyNothing = MyNothing
  fmap f (MyJust x) = MyJust (f x)
  
instance Applicative (MyMaybe) where
  pure = MyJust
  (<*>) _ MyNothing = MyNothing
  (<*>) MyNothing _ = MyNothing
  (<*>) (MyJust f) (MyJust a) = MyJust (f a)

m1 = MyJust (Sum 20)
m2 = MyJust (Sum 30)
m3 = mappend m1 m2 -- == MyJust (Sum 50)

-- ===========================================================
-- Applicatives in Use
-- ===========================================================

-- List Applicative

list1 = [(+1), (*2)] <*> [2, 4] -- == [3,5,4,8]

xs = [1,2,3]
ys = [4,5,6]

x :: Maybe Integer
x = lookup 3 $ zip xs ys

y :: Maybe Integer
y = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = fmap sum $ ((,) <$> x <*> y)

--
-- Identity
--

newtype Identity a = Identity a deriving (Eq, Show)

type Id = Identity

-- Write Applicative instance for Identity

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity (f a)

mkId = Identity

ex1 :: Identity [Integer]
ex1 = const <$> mkId xs <*> mkId ys -- == Identity [1,2,3]

--
-- Constant
--

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

fc = Constant (Sum 1)
gc = Constant (Sum 2)

-- Write an Applicative instance for Constant

instance Functor (Constant a) where
  fmap f (Constant a) = Constant a

instance (Monoid a) => Applicative (Constant a) where
  pure _ = Constant mempty
  (<*>) (Constant a) (Constant a') = Constant (mappend a a')

--
-- Using the Maybe Applicative
--

validateLength :: Int -> String -> Maybe String
validateLength maxLen s = if (length s) > maxLen then Nothing else Just s

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a = fmap Address $ validateLength 100 a

-- Now we'll make a smart constructor for a Person

data Person = Person Name Address deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person
mkPerson n a = case mkName n of
                 Nothing -> Nothing
                 Just n' ->
                   case mkAddress a of
                     Nothing -> Nothing
                     Just a' -> Just $ Person n' a'

str = "Old MaDonald's"

address :: Maybe Address
address = mkAddress str

nm :: Maybe Name
nm = mkName "Ahmed"

pf :: Maybe (Address -> Person)
pf = fmap Person nm

personMaybe :: Maybe Person
personMaybe = pf <*> address

mkPerson' :: String -> String -> Maybe Person
mkPerson' n a = Person <$> mkName n <*> mkAddress a


--

data Cow = Cow {
  name :: String
  , age :: Int
  , weight :: Int
  } deriving (Eq, Show)

cow = Cow { name = "boo", age = 2, weight = 20 }

-- Example of record wild cards
takeCow :: Cow -> Int
takeCow Cow {..} = age * weight

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

nonNegative :: Int -> Maybe Int
nonNegative n | n >= 0 = Just n
              | otherwise = Nothing

-- Validating to get rid of empty string, negative numbers
mkCow :: String -> Int -> Int -> Maybe Cow
mkCow name' age' weight' =
  case noEmpty name' of
    Nothing -> Nothing
    Just nammy ->
      case nonNegative age' of
        Nothing -> Nothing
        Just agey ->
          case nonNegative weight' of
            Nothing -> Nothing
            Just weighty ->
              Just (Cow nammy agey weighty)

mkCow' :: String -> Int -> Int -> Maybe Cow
mkCow' name age weight =
  Cow
  <$> (noEmpty name)
  <*> (nonNegative age)
  <*> (nonNegative weight)

mkCow'' :: String -> Int -> Int -> Maybe Cow
mkCow'' name age weight = liftA3 Cow (noEmpty name) (nonNegative age) (nonNegative weight)

--
-- Exercise
--
-- Given the function values provided, use (<$>) from Functor, (<*>) and pure from Applicative
-- typeclass to fill in missing bits of the broken code to make it work.

-- (1) const <$> Just "Hello" <*> "World"
ex21 = const <$> Just "Hello" <*> pure "World"

-- (2) (,,,) Just 90 <*> Just 10 Just "Tierness" [1,2,3]
ex22 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1,2,3]

-- ===========================================================
-- Applicative Laws
-- ===========================================================

-- (1) Identity

-- pure id <*> v = v

-- e.g:
l11 = pure id <*> [1..5] -- == [1..5]
l12 = pure id <*> Just "Applicative"
l13 = pure id <*> Nothing
l14 = pure id <*> Left "Error"
l15 = pure id <*> Right 42
l16 = pure id <*> (+1) $ 2

-- (2) Composition

-- pure (.) <*> u <*> v <*> w = u <*> (v <*> w)

--e.g:
l21 = pure (.) <*> [(+1)] <*> [(*2)] <*> [1,2,3]  -- compose and then apply
l22 = [(+1)] <*> ([(*2)] <*> [1,2,3])             -- apply and then compose

-- (3) Homomorphism

-- pure f <*> pure x = pure (f x)

l31 = pure (*10) <*> pure 1 :: Sum Int
l32 = pure ((*10) 1) :: Sum Int
      
l33 = pure (*10) <*> pure 1 :: Maybe Int
l34 = pure ((*10) 1) :: Maybe Int

l35 = pure (*10) <*> pure 1 :: [Int]
l36 = pure ((*10) 1) :: [Int]

l37 = pure (*10) <*> pure 1 :: Either a Int
l38 = pure ((*10) 1) :: Either a Int

-- (4) Interchange

-- u <*> pure y = pure ($ y) <*> u
-- u represents a function wrapped in some structure, since on the left of <*> we need that.
--
-- ($ y) is sectioning the $ operator, i.e.
-- ($ 2) is the same as (+10) $ 2

l41 = Just (*10) <*> pure 1
l42 = pure ($ 1) <*> Just (*10)

-- Concretely
mPure :: a -> Maybe a
mPure = pure

embed :: Num a => Maybe ((a -> b) -> b)
embed = mPure ($ 2)

mApply :: Maybe ((a -> b) -> b) -> Maybe (a -> b) -> Maybe b
mApply = (<*>)

myResult = pure ($ 2) `mApply` (Just (+10))

-- Lining up the types for comparision
--
-- <*>    :: Applicative f     => f (x -> y)            -> f x            -> f y
-- mApply ::             Maybe    Maybe ((a -> b) -> b) -> Maybe (a -> b) -> Maybe b
--
-- According to the interchange law, this should be true.
--
l43 = (Just (+2) <*> pure 2)
l44 = pure ($ 2) <*> Just (+2)

-- l43 == l44, since they both do the same thing.

l45 = [(+1), (*2)] <*> pure 10
l46 = pure ($ 10) <*> [(+1), (*2)]

l47 = Just (+3) <*> pure 1
l48 = pure ($ 1) <*> Just (+3)

-- ===========================================================
-- Property Testing Using checkers Library
-- ===========================================================

data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = frequency [ (1, return Fools), (1, return Twoo) ]

instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools

instance EqProp Bull where
  (=-=) = eq

xss :: [(String, String, Int)]
xss = [("b", "w", 1 :: Int)]

yss :: Maybe (Double, [Char], Int)
yss = Just (1.0, "w", 1 :: Int)

main :: IO ()
main = do
  quickBatch (monoid Twoo)
  quickBatch (applicative xss)
  quickBatch (applicative yss)

-- ===========================================================
-- ZipList Monoid
-- ===========================================================

-- The default monoid of lists in GHC Prelude is concatenation.
-- ZipList has an Applicative instance based on zipping

z0 = ZipList [Sum 0]
z1 = ZipList [Sum 1]
z1apply = (\i j -> i * j) <$> z1 <*> z1 -- == [1,4,9]

-- However, there is no instance of Monoid provided, so we need to define one.

-- this isn't going to work properly.
instance Monoid a => Monoid (ZipList a) where
  mempty = pure mempty 
  mappend = liftA2 mappend

-- already defined in Test.QuickCheck.Arbitrary
-- instance Arbitrary a => Arbitrary (ZipList a) where
--  arbitrary = ZipList <$> arbitrary

-- already defined in Test.QuickCheck.Arbitrary
-- instance Arbitrary a => Arbitrary (Sum a) where
--  arbitrary = Sum <$> arbitrary

instance Eq a => EqProp (ZipList a) where
  (=-=) = eq

zl2 = ZipList [1 :: Sum Int]

test :: IO ()
test = do
  quickBatch (monoid zl2)

------------------------------------------------------------
-- Exercise: List Applicative
------------------------------------------------------------

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a xs) = (Cons (f a)) (fmap f xs)

instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  Cons f fs <*> as = append (fmap f as) (fs <*> as)

listGen :: Arbitrary a => Gen (List a)
listGen = do
  a <- arbitrary
  return (Cons a Nil)
  
instance Arbitrary a => Arbitrary (List a) where
  arbitrary = listGen

l1 = Cons 10 Nil
l2 = Cons 20 l1     -- Cons 20 (Cons 10 Nil)
l3 = Cons 30 l2     -- Cons 30 (Cons 20 (Cons 10 Nil))
l2' =  (+1) <$> l2  -- Cons 21 (Cons 11 Nil)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons a xs) ys = Cons a (xs `append` ys)

-- fold right
fold :: (a -> b -> b) -> b -> List a -> b
fold _ acc Nil = acc
fold f acc (Cons x xs) = f x (fold f acc xs)

concat' :: List (List a) -> List a
concat' = fold append Nil

-- write in terms of concat' and fmap
flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' (fmap f as)

toMyList = foldr Cons Nil
xs' = toMyList [1,2,3] -- Cons 1 (Cons 2 (Cons 3 Nil))
c = Cons

f :: Num a => a -> List a
f x = Cons x (Cons 9 Nil)

ff = flatMap f xs'

-- Expected Result
lf = Cons (+1) (Cons (*2) Nil)
lv = Cons 1 (Cons 2 Nil)
lvlf = lf <*> lv -- = Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil)))

-- Compare with: [(+1), (*2)] <*> [1,2] = [2,3,2,4]

apply' :: List (a -> b) -> List a -> List b
apply' Nil _ = Nil
apply' _ Nil = Nil
apply' (Cons f fs) as = append (fmap f as) (apply' fs as)

------------------------------------------------------------
-- ZipList Applicative
------------------------------------------------------------

-- Implement the ZipList Applicative
-- Use the checkers library to validate your Applicative instance

take' :: Int -> List a -> List a
take' i xs = go i xs
  where
    go _ Nil = Nil
    go 0 xs = Nil
    go acc (Cons x xs) = Cons x (go (acc - 1) xs)

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Functor (ZipList') where
  fmap f (ZipList' xs) = ZipList' (fmap f xs)

instance Applicative (ZipList') where
  pure a = ZipList' (repeat' a)
  ZipList' Nil <*> _ = ZipList' Nil
  ZipList' fs  <*> ZipList' vs = ZipList' (zipWith' fs vs)

repeat' :: a -> List a
repeat' x = Cons x (repeat' x)

zipWith' :: List (a -> b) -> List a -> List b 
zipWith' _ Nil = Nil
zipWith' Nil _ = Nil
zipWith' (Cons f fs) (Cons v vs) = Cons (f v) (zipWith' fs vs)

fs = (Cons (+9) (Cons (*2) (Cons (+8) Nil)))
vs = (Cons 1 (Cons 2 (Cons 3 Nil)))
zlf' = ZipList' fs
zlv' = ZipList' vs

zlfzlv = zlf' <*> zlv' -- ZipList' (Cons 10 (Cons 4 (Cons 11 Nil)))

-- Infinite values
zr = ZipList' (repeat' 1)
zlfzr = zlf' <*> zr -- ZipList' (Cons 10 (Cons 2 (Cons 9 Nil)))

-- check Applicative properties using checkers library

instance Arbitrary a => Arbitrary (ZipList' a) where
  -- Note that this will need an Arbitrary instance for the List type.
  arbitrary = fmap ZipList' arbitrary

testZipList :: IO ()
testZipList = do
  quickBatch (applicative (undefined :: ZipList' (Int, Int, Int)))

------------------------------------------------------------
-- Either and Validation Applicative
------------------------------------------------------------

type E = Either

-- <*> :: f   (a -> b) ->   f a ->   f b
-- <*> :: E e (a -> b) -> E e a -> E e b
--
-- pure :: a ->   f a
-- pure :: a -> E e a

-- Often an interesting part of the Applicative is the Monoid.
-- One byproduct of this is that juast a you can have more than one
-- valid Monoid for a given datatype, unlike Functor, Applicative
-- can have more than one valid and lawful instance for a given
-- datatype.

-- Examples
e11 :: Either e Int
e11 = pure 1 :: Either e Int
e12 = Right (+1) <*> Right 2 --- Right (3)
e13 = Right (+1) <*> Left ":(" -- Left ":("
e14 = Left ":(" <*> Right (+1) -- Left ":("
e15 = Left ":(" <*> Left ("boo!") -- Left ":("

-- Validation is similar to Either, but only differs in the Applicative instance

data Validation err a = Failure err | Success a deriving (Eq, Show)

instance Functor (Validation err) where
  fmap f (Success a) = Success (f a)

instance Monoid e => Applicative (Validation e) where
  pure a = Success a
  Success (f) <*> Success (v) = Success (f v)
  Failure e1 <*> Failure e2 = Failure (mappend e1 e2)
  _ <*> Failure err = Failure err
  Failure err <*> _ = Failure err

-- There are natual transformations from Validation to Either and the other way around.

validToEither :: Validation e a -> Either e a
validToEither (Failure e) = Left e
validToEither (Success a) = Right a

eitherToValid :: Either e a -> Validation e a
eitherToValid (Left e) = Failure e
eitherToValid (Right a) = Success a

type Id' = Either Int Int -> Either Int Int
type Id'' = Validation Int Int -> Validation Int Int
testNaturalTransform :: IO ()
testNaturalTransform = do
  let id1 :: Id' = (validToEither . eitherToValid)
  let id2 :: Id'' = (eitherToValid . validToEither)
  return ()

data Errors = DividByZero | StackOverflow deriving (Eq, Show)

-- success = Success (+1) <*> Success (1)

failure = Success (+1) <*> Failure [StackOverflow]
failure' = Failure [StackOverflow] <*> Success (+1)
failures = Failure [DividByZero] <*> Failure[StackOverflow]

------------------------------------------------------------
-- Chapter Exercises
------------------------------------------------------------

-- (1)
type ListType = []

pureList :: a -> [] a
pureList = pure

applyList :: [a -> b] -> [a] -> [b]
applyList = (<*>)

-- (2)
pureIO :: a -> IO (a)
pureIO = pure

applyIO :: IO (a -> b) -> IO (a) -> IO (b)
applyIO = (<*>)
-- applyIO (pure(++ "hello")) getLine

-- (3)
-- (,) a
pureTuple :: Monoid b => a -> (,) b a 
pureTuple = pure
-- pure 1 :: (Sum Int, Int)

applyTuple :: Monoid b => (,) b (a -> b) -> (,) b a -> (,) b b
applyTuple = (<*>)

-- (Sum 10, (+1)) `applyTuple` (Sum 20, 10) -- (Sum {getSum = 30},Sum {getSum = 11})

-- (4)
-- (->) e

pureFun :: a -> (->) e a
pureFun = pure

applyFun :: (->) e (a -> b) -> (->) e a -> (->) e b
applyFun = (<*>)


----

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure x = Two mempty x
  (<*>) (Two u f) (Two v x) = Two (mappend u v) (f x)

pureTwo :: Monoid b => a -> Two b a
pureTwo x = pure x

pureTwoTest = pureTwo 1 :: Two (Sum Int) Int -- pureTwo 1 :: Two (Sum Int) Int

applyTwo :: Monoid b => Two b (a -> b) -> Two b a -> Two b b
applyTwo = (<*>)

applyTwoTest = (Two (Sum 10) (*10)) `applyTwo` (Two (Sum 20) 5) -- Two (Sum {getSum = 30}) 50

instance EqProp (Sum Int) where
  (=-=) = eq

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
twoGen = do
  x <- arbitrary
  y <- arbitrary
  return (Two x y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = twoGen

-- test Applicative properties of Pair

testTwo :: IO ()
testTwo = do
  quickBatch (applicative (undefined :: Two (Sum Int) (Int, Int, Int)))

---------

data Pair a = Pair a a deriving (Eq, Show)

instance Functor (Pair) where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative (Pair) where
  pure a = Pair a a
  (Pair f1 f2) <*> (Pair x y) = Pair (f1 x) (f2 y)

pairGen :: (Arbitrary a) => Gen (Pair a)
pairGen = do
  x <- arbitrary
  return (Pair x x)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = pairGen

instance (Eq a) => EqProp (Pair a) where
  (=-=) = eq

testPair :: IO ()
testPair = do
  quickBatch (functor (undefined :: Pair (Int, Int, Int)))
  quickBatch (applicative (undefined :: Pair (Double, Int, String)))

--------

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure a = Three mempty mempty a
  (Three u v f3) <*> (Three x y z) = Three (u <> x) (v <> y) (f3 z)

threeGen :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
threeGen = do
  x <- arbitrary
  y <- arbitrary
  z <- arbitrary
  return (Three x y z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = threeGen

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

testThree :: IO ()
testThree = do
  quickBatch (functor (undefined :: Three Int Int (Int, Int, Int)))
  quickBatch (applicative (undefined :: Three (Sum Int) (Sum Int) (Int, Int, Int)))

--------

data Three' a b = Three' a b b deriving (Eq, Show)

threeGen' :: (Arbitrary a, Arbitrary b) => Gen (Three' a b)
threeGen' = do
  x <- arbitrary
  y <- arbitrary
  return (Three' x y y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = threeGen'

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance (Monoid a) => Applicative (Three' a) where
  pure x = Three' mempty x x
  (Three' u f1 f2) <*> (Three' v x y) = Three' (u <> v) (f1 x) (f2 y)

testThree' :: IO ()
testThree' = do
  quickBatch (functor (undefined :: Three' Int (Int, Int, Int)))
  quickBatch (applicative (undefined :: Three' (Sum Int) (Int, Int, Int)))

--------

data Four a b c d = Four a b c d deriving (Eq, Show)

fourGen :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Gen (Four a b c d)
fourGen = do
  s <- arbitrary
  t <- arbitrary
  u <- arbitrary
  v <- arbitrary
  return (Four s t u v)
  
instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = fourGen

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure x = Four mempty mempty mempty x
  Four u1 v1 w1 f <*> Four u2 v2 w2 x = Four (u1 <> u2) (v1 <> v2) (w1 <> w2) (f x)

testFour :: IO ()
testFour = do
  quickBatch (functor (undefined :: Four Int Int Int (Int, Int, Int)))
  quickBatch (applicative (undefined :: Four (Sum Int) (Sum Int) (Sum Int) (Int, Int, Int)))

--------

data Four' a b = Four' a a a b deriving (Eq, Show)

fourGen' :: (Arbitrary a, Arbitrary b) => Gen (Four' a b)
fourGen' = do
  s <- arbitrary
  t <- arbitrary
  return (Four' s s s t)
  
instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = fourGen'

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

instance Functor (Four' a) where
  fmap f (Four' a1 a2 a3 b) = Four' a1 a2 a3 (f b)

instance (Monoid a) => Applicative (Four' a) where
  pure x = Four' mempty mempty mempty x
  Four' u1 v1 w1 f <*> Four' u2 v2 w2 x = Four' (u1 <> u2) (v1 <> v2) (w1 <> w2) (f x)

testFour' :: IO ()
testFour' = do
  quickBatch (functor (undefined :: Four' Int (Int, Int, Int)))
  quickBatch (applicative (undefined :: Four' (Sum Int) (Int, Int, Int)))

--------
-- Combinations
--

