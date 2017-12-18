import Data.Monoid as M
import qualified Data.Semigroup as S
import Test.QuickCheck

semiGroupAssoc :: (Eq m, S.Semigroup m) => m -> m -> m -> Bool
semiGroupAssoc a b c = ((a S.<> b) S.<> c) == (a S.<> (b S.<> c))

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

--------------------------------------------------------------------

data Trivial = Trivial deriving (Eq, Show)

-- instance S.Semigroup Trivial where
--  (<>) Trivial Trivial = Trivial
  
instance Monoid (Trivial) where
  mempty = Trivial
  mappend Trivial Trivial = Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivialIdentity = Trivial -> Bool

--------------------------------------------------------------------

newtype Identity a = Identity a deriving (Eq, Show)

instance Monoid a => Monoid (Identity a) where
  mempty = undefined
  mappend (Identity x) (Identity y) = Identity (x <> y)

--------------------------------------------------------------------

main :: IO ()
main = do
  let ma = monoidAssoc
      mli = monoidLeftIdentity
      mri = monoidRightIdentity
  quickCheck (ma :: TrivialAssoc)
  quickCheck (mli :: TrivialIdentity)
  quickCheck (mri :: TrivialIdentity)
  return ()
