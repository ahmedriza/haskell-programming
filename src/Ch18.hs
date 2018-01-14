module Ch18 where

import Control.Monad (join, (>=>))

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

--
-- Chapter 18: Applicative
--

-- =============================================================================
-- The Hierarchy
-- Functor -> Applicative -> Monad
-- =============================================================================

-- class Applicative m => Monad (m :: * -> *) where
--  (>>=) :: m a -> (a -> m b) -> m b
--  (>>) :: m a -> m b -> m b
--  return :: a -> m a


-- We can write fmap in terms of the Monad operators

fmap' f xs = xs >>= return . f

lbind :: [Integer]
lbind = [1..5] >>= (\i -> [i*10]) -- [20,30,40,50,60]

-- 

andOne :: Num t => t -> [t]
andOne x = [x, 1]

-- Note the type similarity to (>>=), but with arguments flipped
--                (a -> f b) -> f a -> f (f b)
fmapAndOne = fmap andOne     [1,2,3] -- [[1,1],[2,1],[3,1]]

-- Now, we have a list of lists. An additional structure has been created.
-- What if we just want a list as the output?

fmapAndOne' = concat $ fmap andOne [1,2,3] -- [1,1,2,1,3,1]

fmapAndOne'' = join $ fmap andOne [1,2,3] -- [1,1,2,1,3,1]

-- Exercise
-- Write bind in terms of fmap and join
--
-- bind :: Monad m => (a -> m b) -> m a -> m b
-- bind f = join . fmap f 

-- bind andOne [1..3] == [1,1,2,1,3,1]
-- [1..3] >>= andOne  == [1,1,2,1,3,1]

g :: Functor f => f String -> f (IO ())
g x = putStrLn <$> x

h :: (String -> b) -> IO b
h x = x <$> getLine

-- -----------------------------------------------------------------------------
-- List Monad
-- ----------------------------------------------------------------------------

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else [x*x]

twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs = xs >>= (\x -> if even x then [x*x, x*x] else [x*x])

-- -----------------------------------------------------------------------------
-- Maybe Monad
-- -----------------------------------------------------------------------------

maybeTest :: Maybe Int
maybeTest = do
  x <- (Just 20)
  y <- (Just 30)
  return (x+y)

maybeTest' = (Just 30)
  >>= (\x -> (Just 20)
        >>= (\y -> return (x + y)))

data Cow = Cow {
  name :: String
  , age :: Int
  , weight :: Int
  } deriving (Show, Eq)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

nonNegative :: Int -> Maybe Int
nonNegative n | n >= 0 = Just n
              | otherwise = Nothing

weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
  in if n == "Bess" && w > 99
     then Nothing
     else Just c

mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' =
  case noEmpty name' of
    Nothing -> Nothing
    Just nammy -> case nonNegative age' of
      Nothing -> Nothing
      Just agey -> case nonNegative weight' of
        Nothing -> Nothing
        Just weighty -> weightCheck (Cow nammy agey weighty)

-- Clean up using do syntax

mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow' name' age' weight' = do
  name <- noEmpty name'
  age <- nonNegative age'
  weight <- nonNegative weight'
  weightCheck (Cow name age weight)

mkSphericalCow'' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow'' name' age' weight' =
  noEmpty name' >>=
  (\name -> nonNegative age' >>=
    (\age -> nonNegative weight' >>=
      (\weight -> weightCheck (Cow name age weight))))

-- -----------------------------------------------------------------------------
-- Either Monad
-- -----------------------------------------------------------------------------

-- (>>=) :: Monad m    => m        a -> (a ->        m b) ->        m b
--                        Either e a -> (a -> Either e b) -> Either e b
--
--return :: Monad m    => a -> m        a
--                        a -> Either e a

-- years ago
type Founded = Int
-- number of programmers
type Coders = Int

data SoftwareShop = Shop {
  founded :: Founded
  , programmers :: Coders
  } deriving (Eq, Show)

data FoundedError = NegativeYears Founded
                    | TooManyYears Founded
                    | NegativeCoders Coders
                    | TooManyCoders Coders
                    | TooManyCodersForYears Founded Coders
                    deriving (Eq, Show)

validateFounded :: Int -> Either FoundedError Founded
validateFounded n
  | n < 0     = Left $ NegativeYears n
  | n > 500   = Left $ TooManyYears n
  | otherwise = Right n

validateCoders :: Int -> Either FoundedError Coders
validateCoders n
  | n < 0     = Left $ NegativeCoders n
  | n > 5000  = Left $ TooManyCoders n
  | otherwise = Right n

mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
mkSoftware years coders = do
  founded <- validateFounded years
  programmers <- validateCoders coders
  if programmers > div founded 10
    then Left $ TooManyCodersForYears founded programmers
    else Right $ Shop founded programmers

--
-- Exercise
--
-- Implement Either Monad

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (First a) = First a
  fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
  pure a = Second a
  First a <*> _ = First a
  _ <*> First a = First a
  Second f <*> Second b = Second (f b)
  
instance Monad (Sum a) where
  First a >>= _ = First a
  Second a >>= f = f a

firstGen :: (Arbitrary a) => Gen (Sum a b)
firstGen = do
  x <- arbitrary
  return (First x)

secondGen :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
secondGen = do
  x <- arbitrary
  return (Second x)
  
instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = frequency [ (1, firstGen), (1, secondGen) ]

instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq

-- =============================================================================
-- Monad Laws
-- =============================================================================

-- (1) Identity
-- Right identity
-- m >>= return = m

-- Left identity
-- return x >>= f = f x

-- (2) Associativity
--
-- (m >>= f) >>= g = m >>= (\x -> f x >>= g)
--

-- A Bad Monad (one which violates Monad laws)

data CountMe a = CountMe Integer a deriving (Eq, Show)

instance Functor CountMe where
  fmap f (CountMe i a) = CountMe (i+1) (f a)

instance Applicative CountMe where
  pure = CountMe 0
  CountMe n f <*> CountMe _ a = CountMe (n + 1) (f a)

instance Monad CountMe where
  return = pure
  CountMe n a >>= f =
    let CountMe _ b = f a
    in CountMe (n + 1) b

instance Arbitrary a => Arbitrary (CountMe a) where
  arbitrary = CountMe <$> arbitrary <*> arbitrary

instance Eq a => EqProp (CountMe a) where
  (=-=) = eq

--
-- Fixing CountMe
--

data CountMe' a = CountMe' Integer a deriving (Eq, Show)

instance Functor CountMe' where
  fmap f (CountMe' i a) = CountMe' i (f a)

instance Applicative CountMe' where
  pure = CountMe' 0
  CountMe' n f <*> CountMe' n' a = CountMe' (n+n') (f a)

instance Monad CountMe' where
  return = pure
  CountMe' n a >>= f =
    let CountMe' n' b = f a
    in CountMe' (n + n') b

instance Arbitrary a => Arbitrary (CountMe' a) where
  arbitrary = CountMe' <$> arbitrary <*> arbitrary

instance Eq a => EqProp (CountMe' a) where
  (=-=) = eq

-- =============================================================================
-- Application and Composition
-- =============================================================================

-- Kleisli Composition

-- (>=>) defined in Control.Monad
--
-- (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c

-- Example

sayHi :: String -> IO String
sayHi greeting = do
  putStrLn greeting
  getLine

readM :: Read a => String -> IO a
readM = return . read

getAge :: String -> IO Int
getAge = sayHi >=> readM

askForAge :: IO Int
askForAge = getAge "Hello! How old are you?"

main :: IO ()
main = do
  -- Test Monad laws for []
  quickBatch (monad (undefined :: [(Int, Int, Int)]))

  -- Test Monad laws for Sum a b
  quickBatch (monad (undefined :: Sum String (Int, Int, Int)))

  -- Test Invalid Monad
  quickBatch $ functor (undefined :: CountMe (Int, Int, Int))
  quickBatch $ applicative (undefined :: CountMe (Int, Int, Int))
  quickBatch $ monad (undefined :: CountMe (Int, Int, Int))

-- =============================================================================
-- Chapter Exercises
-- =============================================================================

-- Write Monad instances for the following types.  Use the QuickCheck properties
-- we showed you to validate your instances.

-- (1)

data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure a = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  _ >>= _ = NopeDotJpg

nopeGen :: Arbitrary a => Gen (Nope a)
nopeGen = do
  return NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = nopeGen

instance (Eq a) => EqProp (Nope a) where
  (=-=) = eq

-- (2)

data Either' b a = Left' a | Right' b deriving (Eq, Show)

-- fmap :: (a -> b) ->         f a ->         f b
-- fmap :: (a -> b) -> Either' b a -> Either' b b

instance Functor (Either' b) where
  fmap f (Left' a) = Left' (f a)
  fmap f (Right' b) = Right' b

instance Applicative (Either' b) where
  pure a = Left' a
  _ <*> Right' b = Right' b
  Right' b <*> _ = Right' b
  Left' f <*> Left' a = Left' (f a)

instance Monad (Either' b) where
  Right' b >>= _ = Right' b
  Left' a >>= f = f a

leftGen :: Arbitrary a => Gen (Either' b a)
leftGen = do
  x <- arbitrary
  return (Left' x)

rightGen :: Arbitrary b => Gen (Either' b a)
rightGen = do
  x <- arbitrary
  return (Right' x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Either' b a) where
  arbitrary = frequency [(1, leftGen), (1, rightGen)]

instance (Eq a, Eq b) => EqProp (Either' b a) where
  (=-=) = eq

-- (3)

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure a = Identity a
  Identity f <*> Identity x =  Identity (f x)

instance Monad Identity where
  return = pure
  Identity x >>= f = f x

idGen :: Arbitrary a => Gen (Identity a)
idGen = do
  x <- arbitrary
  return (Identity x)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = idGen

instance Eq a =>  EqProp (Identity a) where
  (=-=) = eq

-- (4)

data List a = Nil | Cons a (List a)



testCountMe :: IO ()
testCountMe = do
  quickBatch $ functor (undefined :: CountMe' (Int, Int, Int))
  quickBatch $ applicative (undefined :: CountMe' (Int, Int, Int))
  quickBatch $ monad (undefined :: CountMe' (Int, Int, Int))

testNope :: IO ()
testNope = do
  quickBatch $ functor (undefined :: Nope (Int, Int, Int))
  quickBatch $ applicative (undefined :: Nope (Int, Int, Int))
  quickBatch $ monad (undefined :: Nope (Int, Int, Int))

testFlippedEither :: IO ()
testFlippedEither = do
  quickBatch $ functor (undefined :: Either' Int (Int, Int, Int))
  quickBatch $ applicative (undefined :: Either' Int (Int, Int, Int))
  quickBatch $ monad (undefined :: Either' Int (Int, Int, Int))

testIdentity :: IO ()
testIdentity = do
  quickBatch $ functor (undefined :: Identity (Int, Int, Int))
  quickBatch $ applicative (undefined :: Identity (Int, Int, Int))
  quickBatch $ monad (undefined :: Identity (Int, Int, Int))




