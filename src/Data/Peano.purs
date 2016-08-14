module Data.Peano
( Nat(..)
, fromInt
, toInt
) where

import Control.Eval (defer, class Eval, force)
import Prelude
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen (chooseInt)

-- | Peano number with custom evaluation strategy.
data Nat e = Z | S (e (Nat e))

fromInt :: forall e. (Eval e) => Int -> Nat e
fromInt 0 = Z
fromInt n = S (defer \_ -> fromInt (n - 1))

toInt :: forall e. (Eval e) => Nat e -> Int
toInt Z = 0
toInt (S n) = 1 + toInt (force n)

instance semiringNat :: (Eval e) => Semiring (Nat e) where
  zero = Z
  one  = S (defer \_ -> Z)

  add Z     b = b
  add (S a) b = S (defer \_ -> add (force a) b)

  mul Z _ = Z
  mul _ Z = Z
  mul (S a) b = add (mul (force a) b) b

instance eqNat :: (Eval e) => Eq (Nat e) where
  eq Z Z = true
  eq (S a) (S b) = force a == force b
  eq _ _ = false

instance ordNat :: (Eval e) => Ord (Nat e) where
  compare Z Z = EQ
  compare (S _) Z = GT
  compare Z (S _) = LT
  compare (S a) (S b) = force a `compare` force b

instance arbitraryNat :: (Eval e) => Arbitrary (Nat e) where
  arbitrary = fromInt <$> chooseInt 0 4
