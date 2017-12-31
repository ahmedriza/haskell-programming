-- {-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module Ch16 where

import Test.QuickCheck
import Test.QuickCheck.Function
import GHC.Arr

---------------------------
-- Chapter 16
-- Functor
---------------------------
type E e = Either e

--
-- In GHCi, :set -XTypeApplications
-- :type fmap @Maybe
-- :type fmap @(Either _)
--
class (Show a) =>
      OneDigit a where
  hello :: a -> String
  hello = show

instance OneDigit Int

instance OneDigit Double

class Something a where
  s :: a -> a

data MyEither a
  = MyNothing
  | MyJust a deriving (Eq, Show)

instance Functor MyEither where
  fmap f MyNothing  = MyNothing
  fmap f (MyJust a) = MyJust (f a)

data FixMePls a
  = FixMe
  | Pls a
  deriving (Eq, Show)

instance Functor FixMePls where
  fmap _ FixMe   = FixMe
  fmap f (Pls a) = Pls (f a)

-------------------

data CountingBad a = Heisenberg Int a deriving (Eq, Show)

-- NOT OK Functor
instance Functor CountingBad where
  fmap f (Heisenberg n a) = Heisenberg (n+1) (f a)

-- Example
hx = Heisenberg 20 100
f' = (+1)
g' = (+2)

-- The composition law is broken here
comp = fmap (f' . g') hx == (fmap f' . fmap g') hx -- False

-- How do we fix this? Easy. Stop changing anything that's not an argument to f

data CountingGood a = Heisenberg' Int a deriving (Eq, Show)

instance Functor CountingGood where
  fmap f (Heisenberg' n a) = Heisenberg' (n) (f a)
hx' = Heisenberg' 20 100
comp' = fmap (f' . g') hx' == (fmap f' . fmap g') hx' -- True

--------------- Commonly Used Functors ---------------------------------------

replaceWithP :: b -> Char
replaceWithP = const 'p'

-- data Mabye a = Nothing | Just a
m1 = fmap replaceWithP (Just 10) -- (Just 'p')
m2 = fmap replaceWithP Nothing -- Nothing

-- data [] a = [] | a : [a]
a1 = fmap replaceWithP [1,2,3] -- "ppp"
a2 = fmap replaceWithP [] -- ""

-- data (,) a b = (,) a b
p1 = fmap replaceWithP (10, 20) -- (10, 'p')
p2 = fmap replaceWithP (10, "woo") -- (10, 'p')

tossEmOne :: Integer -> Integer
tossEmOne = fmap (+1) negate

t1 = tossEmOne 10 -- -9
t2 = tossEmOne (-10) -- 11

------------

n = Nothing
w = Just "woohoo"
ave = Just "Ave"

lms :: [Maybe String]
lms = [ave, n, w]

lms' :: [Char]
lms' = fmap replaceWithP lms --- "ppp"

lms'' :: [Maybe Char]
lms'' = (fmap . fmap) replaceWithP lms --- [Just 'p',Nothing,Just 'p']

lms''' :: [Maybe String]
lms''' = (fmap . fmap . fmap) replaceWithP lms -- [Just "ppp",Nothing,Just "pppppp"]

-- How does (fmap. fmap) work: 
--
-- (1) function composition works like this for ordinary functions
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- (2) fmap
-- fmap :: Functor f => (m -> n) -> f m -> f n
-- fmap :: Functor g => (x -> y) -> g x -> g y
-- (3)
-- (.) fmap fmap = ?
-- (.) :: (Functor f, Functor g) => (a -> b) -> g (f a) -> g (f b)
--

-----------------

ha :: Maybe [String]
ha = Just ["Ha", "Ha"]

lmls :: [Maybe [String]]
lmls = [ha, Nothing, Just []]

lmls' = (fmap . fmap) replaceWithP lmls -- [Just 'p', Nothing, Just 'p']

tripleFmap = (fmap . fmap . fmap) replaceWithP lmls --- [Just "pp", Nothing, Just ""]

quadrupleFmap = (fmap . fmap . fmap . fmap) replaceWithP lmls -- [Just ["pp", "pp"], Nothing, Just []]

----------------

-- Just making the arguments more specific
replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

-- What happens if we lift it?

liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

-- But we can give a more specific type to liftedReplace
liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

-- What if we lifted twice?
twiceLifted :: (Functor f1, Functor f) => f (f1 a) -> f (f1 Char)
twiceLifted = (fmap . fmap) replaceWithP

-- Making it more specific
twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = twiceLifted

thriceLifted :: (Functor f2, Functor f1, Functor f) => f (f1 (f2 a)) -> f (f1 (f2 Char))
thriceLifted = (fmap . fmap . fmap) replaceWithP

-- Making it more specific
thriceLifted' :: [Maybe [Char]] -> [Maybe [Char]]
thriceLifted' = thriceLifted

-- Now we can print the results from our expressions and compare them

main :: IO ()
main = do
  putStr "replaceWithP' lms:  "
  print (replaceWithP' lms)

  putStr "lifedReplace lms:   "
  print (liftedReplace lms)

  putStr "lifedReplace' lms:  "
  print (liftedReplace' lms)

  putStr "twiceLifted lms:    "
  print (twiceLifted lms)

  putStr "twiceLifted' lms:   "
  print (twiceLifted' lms)

  putStr "thriceLifted lms:   "
  print (thriceLifted lms)

  putStr "thriceLifted' lms:  "
  print (thriceLifted' lms)

  print "done"

-------------------------------------------------------------------------------------------
-- Exercises
-------------------------------------------------------------------------------------------
--
-- Add fmap, parentheses, and function composition to the expression as needed for the expression
-- to typecheck and produce the expected result.

-- (1)
-- Expected result [2]
-- a = (+1) $ read "[1]" :: [Int]
a' = fmap (+1) $ read "[1]" :: [Int]

-- (2)
-- b = (++ "lol") (Just ["Hi,", "Hello"])
-- expected result: Just ["Hi,lol", "Hellolol"]
b' = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

-- (3)
-- c = (*2) (\x -> x - 2)
-- expected output: c 1 = -2
c' :: Integer -> Integer
c' = fmap (*2) (\x -> x - 2)

-- (4)
-- d = ((return '1' ++) . show) (\x -> [x, 1..3])
-- expected output: d 0 = "1[0,1,2,3]"
d' = fmap ((return '1' ++) . show) (\x -> [x, 1..3])

-- (5)
-- e :: IO Integer
-- e = let ioi = readIO "1" :: IO Integer
--         changed = read ("123"++) show ioi
--     in (*3) changed
--
-- expected output: e = 3693

-- ioi = readIO "1" :: IO Integer
-- fmap show ioi = "1" :: IO String
-- x = (fmap ("123" ++) (fmap show ioi)) -- = "1231" :: IO String
-- y = fmap read x :: IO Integer
-- z = fmap (*3) y 

e' :: IO Integer
e' = let ioi = readIO "1" :: IO Integer
         changed :: IO Integer
         changed = fmap read (fmap ("123" ++) (fmap show ioi))
    in fmap (*3) changed
          
e'' :: IO Integer
e'' = let ioi = readIO "1" :: IO Integer
          changed = read <$> (("123" ++) <$> (show <$> ioi))
      in (*3) <$> changed

-- =============================================================================
-- Transforming the Unapplied Type Argument
-- =============================================================================

data Two a b = Two a b deriving (Eq, Show)  -- like (,)

data Or a b = First a | Second b deriving (Eq, Show) -- like Either

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Functor (Or a) where
  fmap f (First a) = First a
  fmap f (Second b) = Second (f b)

-- =============================================================================
-- QuickChecking Functor Instances
-- =============================================================================

-- Functor laws
-- (1) fmap id = id
-- (2) fmap (f . g) = (fmap f) . (fmap g)

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Functor f, Eq (f c)) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

fid :: [Int] -> Bool
fid x = functorIdentity x

fc1 :: Maybe Int -> Bool
fc1 = functorCompose (+1) (+2)

li1 :: Maybe Int -> Bool
li1 x = fc1 (x :: Maybe Int)

fc2 :: [Int] -> Bool
fc2 = functorCompose (+1) (+2)

li2 :: [Int] -> Bool
li2 x = fc2 (x :: [Int])
  
test1 :: IO ()
test1 = do
  quickCheck (functorIdentity :: [Int] -> Bool)
  quickCheck fid
  quickCheck li1
  quickCheck li2

-- =============================================================================
-- Making QuickCheck Generate Functions
-- =============================================================================

functorCompose' :: (Functor f, Eq (f c)) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool

test2 :: IO ()
test2 = do
  quickCheck (functorCompose' :: IntFC)
  
-- =============================================================================
-- Exercises: Instances of Func
-- =============================================================================

-- Implement Functor instances for the following datatypes. 

----- (1) ------
newtype Identity a = Identity a deriving (Eq, Show)
instance Functor (Identity) where
  fmap f (Identity a) = Identity (f a)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  x <- arbitrary
  return (Identity x)
  
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen

type IdentityFC = (Identity Int) -> IntToInt -> IntToInt -> Bool

----- (2) ------

data Pair a = Pair a a deriving (Eq, Show)

instance Functor (Pair) where
  fmap f (Pair a b) = Pair (f a) (f b)

pairGen :: Arbitrary a => Gen (Pair a)
pairGen = do
  x <- arbitrary
  return (Pair x x)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = pairGen

type PairFC = (Pair Int) -> IntToInt -> IntToInt -> Bool  

----- (3) ------

twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
twoGen = do
  x <- arbitrary
  y <- arbitrary
  return (Two x y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = twoGen

type TwoFC = (Two Int Int) -> IntToInt -> IntToInt -> Bool  

----- (4) ------

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

threeGen :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
threeGen = do
  x <- arbitrary
  y <- arbitrary
  z <- arbitrary
  return (Three x y z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = threeGen

type ThreeFC = (Three Int Int Int) -> IntToInt -> IntToInt -> Bool  

----- (5) ------

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

threeGen' :: (Arbitrary a, Arbitrary b) => Gen (Three' a b)
threeGen' = do
  x <- arbitrary
  y <- arbitrary
  return (Three' x y y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = threeGen'
  
type ThreeFC' = (Three' Int Int) -> IntToInt -> IntToInt -> Bool  

----- (6) ------

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

fourGen :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Gen (Four a b c d)
fourGen = do
  s <- arbitrary
  t <- arbitrary
  u <- arbitrary
  v <- arbitrary
  return (Four s t u v)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = fourGen

type FourFC = (Four Int Int Int Int) -> IntToInt -> IntToInt -> Bool  

----- (7) ------

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

fourGen' :: (Arbitrary a, Arbitrary b) => Gen (Four' a b)
fourGen' = do
  s <- arbitrary
  t <- arbitrary
  return (Four' s s s t)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = fourGen'

type FourFC' = (Four' Int Int) -> IntToInt -> IntToInt -> Bool  

--------------

test3 :: IO ()
test3 = do
  quickCheck (functorIdentity :: (Identity Int) -> Bool)
  quickCheck (functorCompose' :: IdentityFC)

  quickCheck (functorIdentity :: (Pair Int) -> Bool)
  quickCheck (functorCompose' :: PairFC)

  quickCheck (functorIdentity :: (Two Int Int) -> Bool)
  quickCheck (functorCompose' :: TwoFC)

  quickCheck (functorIdentity :: (Three Int Int Int) -> Bool)
  quickCheck (functorCompose' :: ThreeFC)

  quickCheck (functorIdentity :: (Three' Int Int) -> Bool)
  quickCheck (functorCompose' :: ThreeFC')

  quickCheck (functorIdentity :: (Four Int Int Int Int) -> Bool)
  quickCheck (functorCompose' :: FourFC)

  quickCheck (functorIdentity :: (Four' Int Int) -> Bool)
  quickCheck (functorCompose' :: FourFC')

-- =============================================================================
-- Ignoring Possibilities
-- =============================================================================

incIfJust :: Num a => Maybe a -> Maybe a
incIfJust (Just n) = Just $ n + 1
incIfJust Nothing  = Nothing

showIfJust :: Show a => Maybe a -> Maybe String
showIfJust (Just s) = Just $ show s
showIfJust Nothing = Nothing

incMaybe :: Num a => Maybe a -> Maybe a
incMaybe m = fmap (+1) m

showMaybe :: Show a => Maybe a -> Maybe String
showMaybe m = fmap show m

incMaybe' :: Num a => Maybe a -> Maybe a
incMaybe' = fmap (+1)

showMaybe' :: Show a => Maybe a -> Maybe String
showMaybe' = fmap show

liftedInc :: (Functor f, Num a) => f a -> f a
liftedInc = fmap (+1)

liftedShow :: (Functor f, Show a) => f a -> f String
liftedShow = fmap show

-- Exercise

-- Write a Functor instance for a datatype identical to Either. We'll use our own
-- datatype

data Sum a b = FirstClass a | SecondClass b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (FirstClass a) = FirstClass a
  fmap f (SecondClass b) = SecondClass (f b)

-- =============================================================================
-- A Somewhat Surpsing Functor
-- =============================================================================

c1 = const 20 30

data Constant a b = Constant { getConstant :: a } deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

c2 = Constant 20

-- =============================================================================
-- More structure, more functors
-- =============================================================================

data Wrap f a = Wrap (f a) deriving (Eq, Show)

instance (Functor f) => Functor (Wrap f) where
  fmap g (Wrap (fa)) = Wrap (fmap g fa)

w1 = Wrap (Just 10)

data Or' a b = First' a | Second' b deriving (Eq, Show)
w2= Wrap (First' 20)

-- =============================================================================
-- IO Functor
-- =============================================================================

ioInt :: IO Int
ioInt = readIO "10" :: IO Int

ioInt' :: IO Int
ioInt' = fmap (+1) ioInt -- equals 11

-- =============================================================================
-- What if we want to do something different?
-- =============================================================================

-- Natural transformation: leave the values untouched, but transform the structure.

type Nat f g = forall a . f a -> g a

-- Example:

maybeToList :: Nat Maybe []
-- maybeToList :: Maybe t -> [t]
maybeToList Nothing = []
maybeToList (Just a) = [a]

-- =============================================================================
-- Functors are unique to a datatype
-- =============================================================================

-- =============================================================================
-- Chapter Exercises
-- =============================================================================

-- Determine if a valid Functor can be written for the datatype provided.

-- (1)
data MyBool = MyFalse | MyTrue
-- Answer: No

-- (2)
data BoolAndSomethingElse a = False' a | True' a

instance Functor (BoolAndSomethingElse) where
  fmap f (False' a) = False' (f a)
  fmap f (True' a) = True' (f a)

-- (3)
data BoolAndMaybeSomethingElse a = Falish | Truish a
instance Functor (BoolAndMaybeSomethingElse) where
  fmap f (Falish) = Falish
  fmap f (Truish a) = Truish (f a)

-- (4) Use kinds to guide you on this one
newtype Mu f = InF { outF :: f (Mu f) }
-- instance Functor (Mu (f a)) where
-- Not possible, since kind of Mu is: (* -> *) -> *

-- (5) Use kinds again
data D = D (Array Word Word) Int Int
-- Not possible, since kind of D is: *

-- Re-arrange arguments to the type constructor of the datatype so that the Functor works.

-- (1)
-- data MySum a b = MyFirst a | MySecond b

data MySum b a = MyFirst a | MySecond b
instance Functor (MySum e) where
  fmap f (MyFirst a) = MyFirst (f a)
  fmap f (MySecond b) = MySecond b

-- (2)
data Company a c b = DeepBlue a c | Something b
instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

-- (3)
-- fmap (+1) (L 1 2 3) == L 2 2 4
-- fmap (+1) (R 1 2 3) == R 1 3 3
data More b a = L a b a | R b a b deriving (Eq, Show)
instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

-- Write Functor instances for the following datatypes

-- (1)
data Quant a b = Finance | Desk a | Bloor b
instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap f (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

-- (2)
-- data K a b = K a

newtype K a b = K a  deriving (Eq, Show)

instance Functor (K a) where
  fmap _ (K a) = K a

-- (3)
newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip $ K (f a)

-- (4)
data EvilGoateeConst a b = GoatyConst b deriving (Eq, Show)
instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

-- (5) Do you need something extra to make the instance work
data LiftItOut f a = LiftItOut (f a) deriving (Eq, Show)

instance Functor f => Functor (LiftItOut f) where
  fmap g (LiftItOut fa) = LiftItOut (fmap g fa)

lifted :: LiftItOut Maybe Integer
lifted = LiftItOut (Just 10)

-- (6)
data DaWrappa f g a = DaWrappa (f a) (g a) deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (DaWrappa f g) where
  fmap h (DaWrappa f g) = DaWrappa (fmap h f) (fmap h g)

dw1 :: DaWrappa Maybe Maybe Integer
dw1 = DaWrappa (Just 10) (Just 20)

dw1' = fmap (+1) dw1 == DaWrappa (Just 11) (Just 21)

-- (7)
data IgnoreOne f g a b = IgnoringSomething (f a) (g b) deriving (Eq, Show)

instance (Functor g) => Functor (IgnoreOne f g a) where
  -- apply h only to the second argument b
  fmap h (IgnoringSomething fa gb) = IgnoringSomething fa (fmap h gb) 

ignore1 = IgnoringSomething (Just 20) (Just 30)

ignore1' = fmap (+1) ignore1

-- (8)
data Notorious g o a t = Notorious (g o) (g a) (g t) deriving (Eq, Show)

instance Functor g => Functor (Notorious g o a) where
  -- apply h only to the 3rd argument t
  fmap h (Notorious go ga gt) = Notorious go ga (fmap h gt)

notorious = Notorious (Just 10) (Just 20) (Just 30)
notorious' = fmap (+1) notorious

-- (9) You'll need to use recursion
data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor (List) where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

list1 = Cons 10 (Nil)
list2 = Cons 20 list1

list2' = fmap (+1) list2 == Cons 21  (Cons 11 Nil)

-- (10) A tree of goats forms a Goat-Lord, fearsome poly-creature

data GoatLord a = NoGoat
                  | OneGoat a
                  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
                  deriving (Eq, Show)

instance Functor (GoatLord) where
  fmap f NoGoat = NoGoat
  fmap f (OneGoat x) = OneGoat (f x)
  fmap f (MoreGoats gl1 gl2 gl3) = MoreGoats (fmap f gl1) (fmap f gl2) (fmap f gl3) 

ng = NoGoat
g1 = OneGoat 1
mg1 = MoreGoats ng (OneGoat 2) (OneGoat 3)
mg2 = MoreGoats g1 g1 mg1 -- == MoreGoats (OneGoat 1) (OneGoat 1) (MoreGoats NoGoat (OneGoat 2) (OneGoat 3))
 
mg2' = fmap (+1) mg2 == MoreGoats (OneGoat 2) (OneGoat 2) (MoreGoats NoGoat (OneGoat 3) (OneGoat 4))

-- (11) You'll use an extra functor for this one, although your solution might do it
-- monomorphically without using fmap.  Keep in mind that you will probably not be able to validate
-- this one in the usual manner.  Do your best to make it work.

instance Show (String -> a) where
  show a = "(String -> a)"

newtype Reader a = Read' (String -> a)
instance Functor (Reader) where
  fmap f (Read' g) = Read' (fmap f g)

apply :: a -> (Reader a) -> a
apply x ra = undefined

r1 = Read' (\s-> read s :: Integer)  

data TalkToMe a = Halt
                  | Print String a
                  | Reader a -- Read' (String -> a)
                  deriving Show

instance Functor (TalkToMe) where
  fmap _ Halt = Halt
  fmap g (Print s x) = Print s (g x)
  -- fmap g (Read' f) = Read' (fmap g f)

-- instance Functor (Read g a) where
--  fmap f (Read g) = undefined
  
tt1 :: TalkToMe a
tt1 = Halt

tt2 :: TalkToMe Integer
tt2 = Print "hello" 10
tt2' = fmap (+1) tt2 -- == Print "hello" 11

-- tt3 :: TalkToMe Integer
-- tt3 = Read' (\s -> read s :: Integer)

-- fmap (+10) (\s -> read s :: Integer) ("11") == 21

-- tt3' :: TalkToMe Integer
--tt3' = fmap (+1) tt3
