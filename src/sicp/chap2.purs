module Sicp.Chap2 where

import Prelude

-- Could also maybe be:
-- newtype Rat = Rat (Tuple Int Int)
--
-- But that seems to create a lot of syntax noise, and since we will not reuse
-- most instances, this is not worth it?
data Rat n d
  = Rat Int Int

makeRat :: Int -> Int -> Rat Int Int
makeRat n d =
  let
    g = gcd n d
  in
    Rat (n / g) (d / g)

numer :: Rat Int Int -> Int
numer (Rat n d) = n

denom :: Rat Int Int -> Int
denom (Rat n d) = d

instance showRat :: Show (Rat Int Int) where
  show (Rat n d) = show n <> "/" <> show d

addRat :: Rat Int Int -> Rat Int Int -> Rat Int Int
addRat x y =
  makeRat
    ((numer x * denom y) + (numer y * denom x))
    (denom x * denom y)

subRat :: Rat Int Int -> Rat Int Int -> Rat Int Int
subRat x y =
  makeRat
    ((numer x * denom y) - (numer y * denom x))
    (denom x * denom y)

mulRat :: Rat Int Int -> Rat Int Int -> Rat Int Int
mulRat x y = makeRat (numer x * numer y) (denom x * denom y)

divRat :: Rat Int Int -> Rat Int Int -> Rat Int Int
divRat x y = makeRat (numer x * denom y) (denom x * numer y)
