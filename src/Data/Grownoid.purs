module Data.Grownoid
( class Grownoid
) where

import Control.Eval (class Eval)
import Data.Monoid (class Monoid)
import Data.Monoid.Additive (Additive)
import Data.Monoid.Disj (Disj)
import Data.Peano (Nat)
import Prelude

-- | Growing monoid.
-- |
-- | Instances must satisfy the following laws in addition to the monoid and
-- | ord laws:
-- |
-- | - `mempty` lower bound: `∀x:m. x >= mempty`
class (Monoid m, Ord m) <= Grownoid m

instance grownoidUnit :: Grownoid Unit
instance grownoidDisjBoolean :: Grownoid (Disj Boolean)
instance grownoidAdditivePeano :: (Eval e) => Grownoid (Additive (Nat e))
instance grownoidArray :: (Ord a) => Grownoid (Array a)
instance grownoidString :: Grownoid String
