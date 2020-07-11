module Sicp.Chap2 where

import Prelude

-- Could also maybe be:
-- newtype Rat = Rat (Tuple Int Int)
--
-- But that seems to create a lot of syntax noise, and since we will not reuse
-- most instances, this is not worth it?
data Rat n d = Rat Int Int

numer :: Rat Int Int -> Int
numer (Rat n d) = n

denom :: Rat Int Int -> Int
denom (Rat n d) = d

instance showRat :: Show (Rat Int Int) where
  show (Rat n d) = show n <> "/" <> show d
