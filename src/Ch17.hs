{-# LANGUAGE RecordWildCards #-}

module Ch17 where
--
-- Chapter 17: Applicative
--

import Control.Applicative
import Data.Monoid

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




