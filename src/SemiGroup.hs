--
-- Chapter 15
-- Semi groups
--

import Data.Semigroup as S
import Data.List.NonEmpty as N

x :: NonEmpty Integer
x = 10 :| []

xx :: NonEmpty Integer
xx = 10 :| [20]

xxx = x <> xx

head_xxx = N.head xx

len_xxx = N.length xxx


