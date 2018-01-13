module Ch18 where

import Control.Monad (join)

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
bind :: Monad m => (a -> m b) -> m a -> m b
bind f = join . fmap f 

-- bind andOne [1..3] == [1,1,2,1,3,1]
-- [1..3] >>= andOne  == [1,1,2,1,3,1]

g :: Functor f => f String -> f (IO ())
g x = putStrLn <$> x

h :: (String -> b) -> IO b
h x = x <$> getLine

-- List Monad

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else [x*x]

twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs = xs >>= (\x -> if even x then [x*x, x*x] else [x*x])

-- Maybe Monad

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


