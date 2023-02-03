module HW0.T6 (a, b, c, a_whnf, b_whnf, c_whnf) where

import Data.Char (isSpace)
import HW0.T1 (distrib)

a :: (Either [Char] b, Either [Char] c)
a = distrib (Left ("AB" ++ "CD" ++ "EF")) -- distrib from HW0.T1

b :: [Bool]
b = map isSpace "Hello, World"

c :: [Char]
c = if 1 > 0 || error "X" then "Y" else "Z"

--here should be sharing. but how to visualise it?
--a_whnf = (Left $ "AB" ++ "CD" ++ "EF", Left $ "AB" ++ "CD" ++ "EF")
a_whnf :: (Either [Char] b1, Either [Char] b2)
a_whnf =
  let left = Left $ "AB" ++ "CD" ++ "EF"
   in (,) left left

b_whnf :: [Bool]
b_whnf = isSpace 'H' : map isSpace "ello, World"

-- 'Y' : [] is absolutely same to ['Y'] (hlint)
c_whnf :: [Char]
c_whnf = ['Y']
