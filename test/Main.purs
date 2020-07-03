module Test.Main where

import Prelude
import Test.QuickCheck (quickCheck, (<?>))
import Data.Array (foldr, zip)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Htdp.PartThree (add1)

main :: Effect Unit
main = do
  testAdd1

-- it's a stupid test, I'm just trying it out
testAdd1 :: Effect Unit
testAdd1 = do
  quickCheck \xs -> -- generates values automatically based on the type
    let
      incremented = (add1 xs)
    in
      all isOneMore (zip incremented xs)
        <?> "Result:\n"
        <> show incremented
        <> "\nnot equal to expected:\n"
        <> show (map ((+) 1) xs)

-- probably this exists in some common module?
-- this is basically Array.every
all :: ∀ a. (a -> Boolean) -> Array a -> Boolean
all p xs = foldr (\x acc -> acc && p x) true xs

isOneMore :: Tuple Int Int -> Boolean
isOneMore (Tuple x y) = eq x (y + 1)
