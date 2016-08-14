module Test.Data.Peano
( main
) where

import Control.Eval (Lazy, Strict)
import Data.Peano (Nat(..))
import Prelude
import Test.QuickCheck.Laws.Data (checkEq, checkOrd, checkSemiring)
import Type.Proxy (Proxy(Proxy))

main = do
  main' (Proxy :: Proxy (Nat Lazy))
  main' (Proxy :: Proxy (Nat Strict))

main' p = do
  checkSemiring p
  checkEq p
  checkOrd p
