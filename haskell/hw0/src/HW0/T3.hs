module HW0.T3 (s, k, i, compose, contract, permute) where

-- s :: (a -> b -> c) -> (a -> b) -> (a -> c)
-- f = (a -> b -> c)
-- g = (a -> b)
-- x = a
-- f x (g x) = f a b = c
s :: (a -> b -> c) -> (a -> b) -> (a -> c)
s f g x = f x (g x)

k :: a -> b -> a
k x _ = x

-- i :: a -> a
-- s k = (a=a -> b=b -> c=a) -> (a -> b) -> (a -> a)
-- s k = (a -> b) -> (a -> a)
-- s k k = (a=a -> b=b->a) -> (a -> a)
-- s k k = a -> a
i :: a -> a
i = s k k

-- compose :: (b -> c) -> (a -> b) -> (a -> c)
-- x = b -> c
-- y = a -> b
-- (x (y a)) -> ret c
-- compose x y a = x (y a)
-- no parameters allowed, try again!
-- x = b -> c
-- y = a -> b
-- z = a
-- k = a -> b -> a
-- s = (a -> b -> c) -> (a -> b) -> (a -> c)
-- k s = a={(a->b->c)->(a->b)->(a->c)} -> b -> a
-- k s = b' -> (a -> b -> c) -> (a -> b) -> (a -> c)
-- s (k s) = (a={b'} -> b={(a->b->c)} -> c={(a->b)->(a->c)}) -> (a -> b) -> (a -> c)
-- s (k s) = (b' -> a -> b -> c) -> b' -> (a -> b) -> (a -> c)
-- s (k s) = (b' -> a -> (b -> c)) -> b' -> (a -> b) -> (a -> c)
-- s (k s) k = (b'={a2} -> a={b2} -> (b -> c)={a2}) -> b' -> (a -> b) -> (a -> c)
-- s (k s) k = (b -> c) -> (b2 -> b) -> (b2 -> c)
-- this is it.
compose :: (b -> c) -> (a -> b) -> (a -> c)
compose = s (k s) k

-- contract :: (a -> a -> b) -> (a -> b)
-- x = a -> a -> b
-- (x a a) = b
-- contract f a = f a a
-- try again, but without parameters
-- k = a -> b -> a
-- s = (a -> b -> c) -> (a -> b) -> (a -> c)
-- s s = (a=(a2->b2->c2) -> b=(a2->b2) -> c=(a2->c2)) -> (a -> b) -> (a -> c)
-- s s = ((a2 -> b2 -> c2) -> (a2 -> b2)) -> ((a2 -> b2 -> c2) -> (a2 -> c2))
-- s s = ((a2 -> b2 -> c2) -> a2 -> b2) -> (a2 -> b2 -> c2) -> (a2 -> c2)
-- s k = (a -> b) -> (a -> a)
-- s k = (a -> b) -> a -> a
-- s s (s k) = ((a2 -> b2 -> c2)={a->b} -> a2={a} -> b2={a}) -> (a2 -> b2 -> c2) -> (a2 -> c2)
-- s s (s k) = ((a2={a} -> (b2 -> c2)={b}) -> a2={a} -> b2={a}) -> (a2 -> b2 -> c2) -> (a2 -> c2)
-- s s (s k) = (a -> a -> c2) -> (a -> c2)
contract :: (a -> a -> b) -> (a -> b)
contract = s s (s k)

-- permute :: (a -> b -> c) -> (b -> a -> c)
-- x = a -> b -> c
-- y = b -> a -> c
-- k = a -> b -> a
-- s = (a -> b -> c) -> (a -> b) -> (a -> c)
-- compose = (b -> c) -> (a -> b) -> (a -> c)
-- k k = a={a2 -> b2 -> a2} -> b -> a
-- k k = b -> a2 -> b2 -> a2
-- k compose = a={(b2 -> c2) -> (a2 -> b2) -> (a2 -> c2)} -> b -> a
-- k compose = b -> (b2 -> c2) -> (a2 -> b2) -> (a2 -> c2)
-- s (k compose) = (a={b1} -> b={b2 -> c2} -> c={(a2 -> b2) -> (a2 -> c2)}) -> (a -> b) -> (a -> c)
-- s (k compose) = (b1 -> b2 -> c2) -> b1 -> (a2 -> b2) -> a2 -> c2
-- s (k compose) s = (b1={a -> b -> c} -> b2={a -> b} -> c2={a -> c}) -> b1 -> (a2 -> b2) -> a2 -> c2
-- s (k compose) s = (a -> b -> c) -> (a2 -> a -> b) -> a2 -> a -> c
-- s (s (k compose) s) = (a={a1 -> b1 -> c1} -> b={a2 -> a1 -> b1} -> c={a2 -> a1 -> c1}) -> (a -> b) -> (a -> c)
-- s (s (k compose) s) = ((a1 -> b1 -> c1) -> a2 -> a1 -> b1) -> (a1 -> b1 -> c1) -> a2 -> a1 -> c1
-- s (s (k compose) s) (k k) = ((a1 -> b1 -> c1) -> a2 -> a1 -> b1)={b -> a -> b' -> a} -> (a1 -> b1 -> c1) -> a2 -> a1 -> c1
-- s (s (k compose) s) (k k) = ((a1 -> b1 -> c1)={b} -> a2={a} -> a1={b'} -> b1={a}) -> (a1 -> b1 -> c1) -> a2 -> a1 -> c1
-- s (s (k compose) s) (k k) = (b' -> a -> c) -> a -> b' -> c
permute :: (a -> b -> c) -> (b -> a -> c)
permute = s (s (k (s (k s) k)) s) (k k)
