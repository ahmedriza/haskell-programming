--
-- Chapter 21 - Traversable
--

module Ch21 where

import Data.Traversable
import qualified GHC.List as List ( foldr )

import qualified Data.Map as M -- used in the Morse Code Example
import Data.Maybe              -- used in the Morse Code Example

-- For section 21.8
import Data.ByteString.Lazy hiding (map)
import Network.Wreq

import Data.Functor.Identity
import Data.Monoid
import Data.Functor.Constant

-- sequenceA :: Applicative f => t (f a) -> f (t a)
-- sequenceA = traverse id

traverse' :: (
  Functor t
  , Foldable t
  , Traversable t
  , Applicative f
  )
  => (a -> f b) -> t a -> f (t b)
traverse' f = sequenceA . fmap f

traverse'' :: Applicative f => (t -> f a) -> [t] -> f [a]
traverse'' f xs = List.foldr cons_f (pure []) xs
  where
    cons_f x ys = (:) <$> f x <*> ys
  

f :: Int -> IO (Int)
f x = return (x*x)

g :: IO (Int)
g = return 20

list1 = [1,2,3]

list1_t :: IO [Int]
list1_t = traverse'' f list1 

s1 = sum [1,2,3]                           -- 6
s2 = fmap sum [Just 1, Just 2, Just 3]     -- [1,2,3]
s3 = (fmap . fmap) sum Just[1, 2, 3]       -- Just 6
p1 = fmap product [Just 1, Just 2, Just 3] -- [1,2,3]

j1 :: [Maybe Integer]
j1 = fmap Just [1,2,3]                     -- [Just 1, Just 2, Just 3]

-- sequenceA :: (Applicative f, Traversable t) => t (f a) -> f (t a)

seq1 :: Maybe [Integer]
seq1 = sequenceA $ fmap Just [1,2,3]       -- Just [1,2,3]

seq2 = sequenceA [Just 1, Just 2, Just 3]  -- Just [1,2,3]
seq3 = sequenceA [Just 1, Just 2, Nothing] -- Nothing

s4 = fmap sum $ sequenceA [Just 1, Just 2, Just 3] -- Just 6

p2 = fmap product $ sequenceA [Just 3, Just 4, Nothing] -- Nothing

--
-- traverse
--
-- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
-- traverse = sequenceA . fmap f

-- fmap :: Functor f => (a -> b) -> f a -> f b

-- (=<<) :: Monad m => (a -> m b) -> m a -> m b

-- sequenceA :: Applicative f => t (f a) -> f (t a)

t1 = fmap Just [1,2,3]               -- [Just 1, Just 2, Just 3]
t2 = sequenceA $ fmap Just [1,2,3]   -- Just [1,2,3]
t3 = sequenceA . fmap Just $ [1,2,3] -- Just [1,2,3]
t4 = traverse Just [1,2,3]           -- Just [1,2,3]

-- mapM is similar to traverse

-- mapM     :: (Monad m, Traversable t)       => (a -> m b) -> t a -> m (t b)
-- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)

-- Whilst mapM works with Monads, traverse only needs an Applicative
-- Also note that Vector has a Traversable instance, so for performance sensitive code, we can
-- use Vector instead of list ([]).

t5 = mapM Just [1,2,3]               -- Just [1,2,3]

--------------------------------------------------------------------------------
-- 21.5 So, what's Traversable for?
--------------------------------------------------------------------------------

ft :: Int -> Maybe Int
ft x = Just x

xs :: [a]
xs = undefined

ft' :: [Maybe Int]
ft' = map ft xs

-- But what if we want a value of type Maybe [b]?

ft'' :: Maybe [Int]
ft'' = sequenceA $ map ft xs

ft''' :: Maybe [Int]
ft''' = traverse ft xs

--------------------------------------------------------------------------------
-- 21.6 Morse Code Revisited
--------------------------------------------------------------------------------

type Morse = String

letterToMorse :: (M.Map Char Morse)
letterToMorse = M.fromList [
  ('a', ".-")
  , ('b', "-...")
  , ('c', "-.-.")
  , ('d', "-..")
  , ('e', ".")
  , ('f', "..-.")
  , ('g', "--.")
  , ('h', "....")
  , ('i', "..")
  , ('j', ".---")
  , ('k', "-.-")
  , ('l', ".-..")
  , ('m', "--")
  , ('n', "-.")
  , ('o', "---")
  , ('p', ".--.")
  , ('q', "--.-")
  , ('r', ".-.")
  , ('s', "...")
  , ('t', "-")
  , ('u', "..-")
  , ('v', "...-")
  , ('w', ".--")
  , ('x', "-..-")
  , ('y', "-.--")
  , ('z', "--..")
  , ('1', ".----")
  , ('2', "..---")
  , ('3', "...--")
  , ('4', "....-")
  , ('5', ".....")
  , ('6', "-....")
  , ('7', "--...")
  , ('8', "---..")
  , ('9', "----.")
  , ('0', "-----")
  ]

-- morseToLetter :: M.Map Morse Char
morseToLetter = M.foldWithKey (flip M.insert) M.empty letterToMorse

charToMorse :: Char -> Maybe Morse
charToMorse c = M.lookup c letterToMorse

stringToMorse :: String -> Maybe [Morse]
stringToMorse s = traverse charToMorse s

morseToChar :: Morse -> Maybe Char
morseToChar m = M.lookup m morseToLetter

morse :: String -> [Morse]
morse s = fromMaybe [] (stringToMorse s)

morseEx1 :: [Morse]
morseEx1 = morse "ahmed"  -- [".-","....","--",".","-.."]

morseEx2 = fmap morseToChar (morse "ahmed")  -- [Just 'a',Just 'h',Just 'm',Just 'e',Just 'd']

-- sequence :: (Monad m, Traversable t) => t (m a) -> m (t a)
-- Use sequence to transform the output frmo morseEx2

morseEx3 = sequence $ fmap morseToChar (morse "ahmed")         -- Just "ahmed"
morseEx3' = ((sequence . ) . fmap) morseToChar (morse "ahmed") -- Just "ahmed"

-- ((sequence . ) . fmap) is equivalent to traverse
morseEx4 = traverse morseToChar (morse "ahmed")                -- Just "ahmed"

--------------------------------------------------------------------------------
-- 21.7 Axing tedious code
--------------------------------------------------------------------------------

data Query     = Query
data SomeObj   = SomeObj
data IoOnlyObj = IoOnlyObj
data Err       = Err

-- There's a decoder function that makes some object from String
decodeFn :: String -> Either Err SomeObj
decodeFn = undefined

-- There's a query, that runs against the DB and returns an array of strings
fetchFn :: Query -> IO [String]
fetchFn = undefined

-- An additional "context initalizer", that also has IO
makeIoOnlyObj :: [SomeObj] -> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined

-- Before the cleanup

pipelineFn :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn query = do
  a <- fetchFn query
  let s = traverse decodeFn a
  case sequence (map decodeFn a) of
    (Left err) -> return $ Left $ err
    (Right res) -> do
      a <- makeIoOnlyObj res
      return $ Right a

pipelineFn' :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn' query = do
  a <- fetchFn query
  traverse makeIoOnlyObj $ mapM decodeFn a

pipelineFn'' :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn'' query = (traverse makeIoOnlyObj . mapM decodeFn =<<) $ fetchFn query

-- mapM          :: (a     -> m                b) -> t a      -> m          (t b)
-- mapM decodeFn :: String -> Either Err SombeObj -> t String -> Either Err (t SomeObj)
--               ::                               -> t String -> Either Err (t SomeObj)
-- (=<<)               :: (a -> m b)            -> m          a           -> m          b
-- (mapM decodeFn =<<) ::                       Either Err    (t String)  -> Either Err (t SomeObj)

-- traverse                :: (a        -> f  b                    ) -> t a          -> f  (t b)
-- (traverse makeIoOnlyObj):: [SomeObj] -> IO [(SomeObj, IoOnlyObj)] -> t [Sombeobj] -> IO (t [(SomeObj, IoOnlyObj)])
--                                                                   -> t [Sombeobj] -> IO (t [(SomeObj, IoOnlyObj)])

-- g . f : ?
-- Either Err (t String) -> IO (t [(SomeObj, IoOnlyObj)])
--

-- temp query = mapM decodeFn =<< (fetchFn query)

ff :: String -> IO String
ff name = return $ "hi " ++ name

gg :: String -> IO (String, Int)
gg greeting = return (greeting, 20)

test name = do
  f <- ff name
  g <- gg f
  print g

test' = getLine >>=
        (\name -> ff name >>=
                  (\gr -> gg gr))

f' :: Int -> Either Err String
f' x = return $ show x

g' :: IO String -> IO Double
g' = undefined

h' :: IO [Int]
h' = return [1, 2, 3]

fg :: Int -> Int
fg x = x + 1

-- ((->) r) has a Monad instance

-- (>>=) :: m           a -> (a -> m b) -> m b
--         ((->) Int) Int -> (Int -> (->) Int b) -> (->) Int b
--                        -> (Int -> Int -> b)   -> Int -> b

fg' :: Int -> Int -> Double
fg' = undefined

-- (>>=) :: m            a              -> (a -> m b) -> m b
--         ((->) Int) -> Int -> Double) -> (Int -> Double -> (->) Int b) -> Int -> b
--                                      -> ((Int -> Double) -> Int -> b) -> Int -> b

-- (=<<) :: (a   -> m               b) -> m a          -> m b
--          (Int -> ((->) Int) Double) -> (->) Int Int -> (->) Int Double
--                                     -> (Int -> Int) -> Int -> Double

-- mapM :: (a   -> m               b) -> t a   -> m          (t b     )
-- mapM f'
-- mapM    (Int -> Either Err String)
--         (Int -> Either Err String) -> t Int -> Either Err (t String)
--                                    -> t Int -> Either Err (t String)

-- (=<<) :: Monad m => (a -> m b) -> m a -> m b

k' :: [String] -> IO [String]
k' = undefined

-- traverse      :: (a       -> f        b) -> t a  -> f  (t b)

-- (traverse k') :: [String] -> IO [String] ->
--                                          -> t [String] -> IO (t [String])

-- (traverse k' . mapM f')     :: [Int]    -> IO (Either Err [String])
-- (=<<) :: Monad m =>            (a       -> m  b) -> m a -> m b

-- (traverse k' . mapM f') =<< :: IO [Int] -> IO (Either Err [String])


--------------------------------------------------------------------------------
-- 21.8 Do All the Things
--------------------------------------------------------------------------------

urls :: [String]
urls = [ "http://httpbin.org/ip"
       , "http://httpbin.org/bytes/5"
       ]

-- get :: String -> IO (Response ByteString)

mappingGet :: [IO (Response ByteString)]
mappingGet = map get urls

-- But what if we don't want a list of IO actions we can perform to get
-- a response, but rather one big IO action that produces a list of responses?
-- 

mappingGet' :: IO [Response ByteString]
mappingGet' = traverse get urls

pm :: IO (Response ByteString) -> IO (Response ByteString)
pm r = do
  r' <- r
  return r'

testMapping :: IO [Response ByteString]
testMapping = forM mappingGet pm

--------------------------------------------------------------------------------
-- Strength for Understanding
--------------------------------------------------------------------------------

-- Traversable is stronger than Functor and Foldable. Because of this, we can
-- recover the Functor and Foldable instance for a type from the Traversable, just
-- as we can recover the Functor and Applicative from the Monad. 

-- This behaves somewhat like fmap:
u1 :: Identity [Integer]
u1 = traverse (Identity . (*10)) [1,2] -- Identity [10,20]

u1' :: [Integer]
u1' = runIdentity u1 -- [10, 20]

edgeMap :: Traversable t => (a -> b) -> t a -> t b
edgeMap f t = runIdentity $ traverse (Identity . f) t

u1'' = edgeMap (*10) [1..5] -- [10, 20, 30, 40, 50]

-- Using Const or Constant we can recover foldMappy-looking Foldable as well

xs' :: [Sum Integer]
xs' = [0,1,2,3,4,5,6,7,8,9] 

u2 = traverse (Constant . (+1)) xs'  -- 55

foldMap' :: (Monoid a, Traversable t) => (a1 -> a) -> t a1 -> a
foldMap' f t = getConstant $ traverse (Constant . f) t

--------------------------------------------------------------------------------
-- 21.9 Traversable Instances
--------------------------------------------------------------------------------

-- Either

data Or a b = Lefty a | Righty b deriving (Eq, Ord, Show)

instance Functor (Or a) where
  fmap _ (Lefty x) = Lefty x
  fmap f (Righty y) = Righty (f y)

instance Applicative (Or a) where
  pure = Righty
  Lefty x <*> _ = Lefty x
  Righty f <*> r = fmap f r

instance Foldable (Or a) where
  foldMap f (Lefty x) = mempty
  foldMap f (Righty y) = f y

instance Traversable (Or a) where
  traverse g (Lefty x) = pure $ Lefty x
  traverse g (Righty y) = fmap Righty (g y)
  
fm_or1 = fmap (+1) $ Lefty 10
fm_or2 = fmap (+1) $ Righty 10


-- Tuple

data Tuple a b = Tuple a b deriving (Eq, Ord, Show)

instance Functor (Tuple a) where
  fmap f (Tuple x y) = Tuple x (f y)

instance Monoid a => Applicative (Tuple a) where
  pure x = Tuple mempty x
  Tuple u f <*> Tuple v y = Tuple (u `mappend` v) (f y)

instance Foldable (Tuple a) where
  foldMap f (Tuple x y) = f y

instance Traversable (Tuple a) where
  -- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
  traverse g (Tuple x y) = fmap (Tuple x) (g y)

--------------------------------------------------------------------------------
-- 21.10 Traversable Laws
--------------------------------------------------------------------------------

-- The traverse function must satisfy the following laws.

-- (1) Naturality
-- t . traverse f = traverse (t . f)

t :: (Applicative _f, Applicative _g) => _f a -> _g a
t = undefined

tfr :: (Applicative f) => a -> f b
tfr = undefined

tfl :: (Traversable t, Applicative f) => t a -> f (t b)
tfl = undefined

