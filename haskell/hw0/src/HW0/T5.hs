module HW0.T5 (Nat, nz, ns, nplus, nmult, nFromNatural, nToNum) where

import GHC.Natural (Natural)

-- Church numerals
type Nat a = (a -> a) -> a -> a

-- (a -> a) -> a -> a == (a -> a) -> (a -> a)

nz :: Nat a
nz _ a = a

-- ns :: ((a -> a) -> a -> a) -> (a -> a) -> a -> a
-- n = ((a -> a) -> a -> a)
-- f = (a -> a)
-- x = a
-- return a
ns :: Nat a -> Nat a
ns n f x = f $ n f x

nplus :: Nat a -> Nat a -> Nat a
nplus n m f x = n f $ m f x

--nmult n m f = n $ m f
--nmult n m f = n . m $ f
--nmult n m = n . m
--oops :D
nmult :: Nat a -> Nat a -> Nat a
nmult = (.)

nFromNatural :: Natural -> Nat a
nFromNatural 0 = nz
nFromNatural n = \f x -> f (nFromNatural (n - 1) f x)

nToNum :: Num a => Nat a -> a
nToNum f = f (+ 1) 0 -- do +1 n times
