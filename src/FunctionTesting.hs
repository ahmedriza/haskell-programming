import Data.Semigroup
import Test.QuickCheck
-- import Test.QuickCheck.Function
import Text.Show.Functions

import Codec.Picture

instance Arbitrary PixelRGB8 where
  arbitrary = PixelRGB8 <$> arbitrary <*> arbitrary <*> arbitrary

