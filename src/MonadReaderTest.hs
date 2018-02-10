{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module MonadReaderTest where

import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.Trans.Reader

class Monad m => MonadReader' r m | m -> r where
  ask :: m r
  local :: (r -> r) -> m a -> ma

-- Monad instance for ((->) r)
--
-- (>>=) :: Monad m => m        a -> (a -> m        b) -> m        b
-- (>>=)               ((->) r) a -> (a -> ((->) r) b) -> ((->) r) b
-- (>>=)               (-> r   a) -> (a -> (r -> b   ) -> (r -> b)
-- (>>=)               (r -> a) -> (a -> r -> b) -> (r -> b)

f :: Int -> Int
f r = r + 1

g :: Int -> Int -> Int
g a r = a * r

h :: Int -> Int -> Int
h b r = b * r

fg = (f >>= g) 10 -- (10 + 1) * 10

