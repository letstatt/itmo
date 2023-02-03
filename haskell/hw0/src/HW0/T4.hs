module HW0.T4 (repeat', map', fib, fac) where

import Data.Function (fix)
import Numeric.Natural (Natural)

-- fix: (a -> a) -> a
-- fix f = {let x = f x} in x

-- x = f x
-- x = f (f x)
-- x = f (f (f x))
-- oh, pretty recursion

repeat' :: a -> [a] -- behaves like Data.List.repeat
repeat' = fix . (:)

map' :: (a -> b) -> [a] -> [b] -- behaves like Data.List.map
map' f =
  fix
    ( \rec list -> case list of
        (x : xs) -> f x : rec xs
        []       -> []
    )

fib :: Natural -> Natural -- computes the n-th Fibonacci number
fib = fix (\rec a b depth -> if depth == 0 then a else rec b (a + b) (depth - 1)) 0 1

fac :: Natural -> Natural -- computes the factorial
fac = fix (\rec n -> if n == 0 then 1 else n * rec (n - 1))
