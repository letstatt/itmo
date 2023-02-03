module HW0.T2 (Not, doubleNeg, reduceTripleNeg) where

import Data.Void (Void)

type Not a = a -> Void

-- doubleNeg :: a -> Not (Not a)
-- a -> Not (a -> Void)
-- a -> (a -> Void) -> Void
doubleNeg :: a -> Not (Not a)
doubleNeg a f = f a

-- reduceTripleNeg :: Not (Not (Not a)) -> Not a
-- Not (Not (Not a)) -> Not a
-- Not (Not (a -> Void)) -> (a -> Void)
-- Not ((a -> Void) -> Void) -> (a -> Void)
-- (((a -> Void) -> Void) -> Void) -> (a -> Void)
-- (((a -> Void) -> Void) -> Void) -> a -> Void
-- f a = (((a -> Void) -> Void) -> Void)
-- f a = ((a -> Void) -> Void) -> Void
-- f a = g -> Void
-- g a = (a -> Void) -> Void
-- g a = doubleNeg a
-- f a = (doubleNeg) -> Void
reduceTripleNeg :: Not (Not (Not a)) -> Not a
reduceTripleNeg f a = f (doubleNeg a)
