module HW1.T2
  ( N (..),
    nplus,
    nmult,
    nsub,
    ncmp,
    nFromNatural,
    nToNum,
    nEven,
    nOdd,
    ndiv,
    nmod,
  )
where

import GHC.Natural (Natural)

data N = Z | S N

nplus :: N -> N -> N -- addition
nplus Z     = id
nplus (S n) = nplus n . S -- thanks to hlint

nmult :: N -> N -> N -- multiplication
nmult Z _     = Z
nmult _ Z     = Z
nmult (S Z) b = b
nmult a (S Z) = a
nmult a (S n) = nplus a $ nmult a n

nsub :: N -> N -> Maybe N -- subtraction     (Nothing if result is negative)
nsub Z Z         = Just Z
nsub a Z         = Just a
nsub Z _         = Nothing
nsub (S a) (S b) = nsub a b

ncmp :: N -> N -> Ordering -- comparison      (Do not derive Ord)
ncmp a b = checker $ nsub a b
  where
    checker z = case z of
      Nothing -> LT
      Just Z  -> EQ
      Just _  -> GT

nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural a = S $ nFromNatural $ a - 1

nToNum :: Num a => N -> a
nToNum Z     = 0
nToNum (S a) = 1 + nToNum a

nEven :: N -> Bool -- parity checking
nEven Z         = True
nEven (S Z)     = False
nEven (S (S a)) = nEven a

nOdd :: N -> Bool
nOdd a = not $ nEven a

ndiv :: N -> N -> N -- integer division
ndiv a b = rec $ nsub a b
  where
    rec z = case z of
      Nothing -> Z -- zero
      Just Z  -> S Z -- one
      Just c  -> S $ ndiv c b

nmod :: N -> N -> N -- modulo operation
nmod a b = rec $ nsub a b
  where
    rec z = case z of
      Nothing -> a -- zero
      Just Z  -> Z -- one
      Just c  -> nmod c b
