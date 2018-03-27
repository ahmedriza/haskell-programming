module Kleisli where

import Control.Monad

g :: Monad m => a -> m b
g = undefined

h :: Monad m => b -> m c
h = undefined

-- g >=> h :: Monad m => a -> m c
-- h <=< g :: Monad m => a -> m c

-- g a :: m b
-- m b >>= (b -> m c)
--                     -> m c

-- Derivation:
--------------

-- fmap :: Functor f =>
--         (a -> b)   -> f a -> f b
-- fmap    h
-- fmap    (b -> m c) -> f b -> f (m c)
-- fmap    h  ::      -> f b -> f (m c)

-- fmap h . g :: (f b -> f (m c)) . (a -> m b)
-- fmap h . g :: a -> f (m c)

hh :: (Monad f, Monad m) => a -> f (m c)
hh = fmap h . g

hhh :: Monad m => a -> m c
hhh = join hh

-- join :: Monad m => m       (m     a) -> m a
--                    a ->     f (m c)
--                    ((->) a) f (m c)
--                    ((->) a) f (m c) -> ((->) a) (m c)
--                                      -> a -> m c

data Complex = Complex

x :: Int -> String
x = undefined

y :: String -> Double
y = undefined

z :: Double -> Complex
z = undefined

cmp :: Int -> Complex
cmp = (z . y . x)
