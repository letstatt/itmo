{-# LANGUAGE ScopedTypeVariables #-}

module HW2.T2
  ( distOption,
    wrapOption,
    distPair,
    wrapPair,
    distQuad,
    wrapQuad,
    distAnnotated,
    wrapAnnotated,
    distExcept,
    wrapExcept,
    distPrioritised,
    wrapPrioritised,
    distStream,
    wrapStream,
    distList,
    wrapList,
    distFun,
    wrapFun,
  )
where

import HW2.T1

-- For each type from the first task except Tree, implement functions of the following form:
-- distF :: (F a, F b) -> F (a, b)
-- wrapF :: a -> F a

-- You must implement these functions by hand, using only:
-- data types that you defined in HW2.T1
-- (<>) and mempty for Annotated

-- distPrioritised must pick the higher priority out of the two.
-- distList must associate each element of the first list with each element
-- of the second list (i.e. the resulting list is of length n × m).

-- The following laws must hold:
-- Homomorphism:
-- distF (wrapF a, wrapF b)  ≅  wrapF (a, b)

-- Associativity:
-- distF (p, distF (q, r))   ≅  distF (distF (p, q), r)
-- ((a, b), c)  ≅  (a, (b, c))

-- Left and right identity:
-- distF (wrapF (), q)  ≅  q
-- ((), b)  ≅  b

-- distF (p, wrapF ())  ≅  p
-- (a, ())  ≅  a

distOption :: (Option a, Option b) -> Option (a, b)
distOption (Some a, Some b) = Some (a, b)
distOption _                = None

wrapOption :: a -> Option a
wrapOption a = Some a

distPair :: (Pair a, Pair b) -> Pair (a, b)
distPair (P a b, P c d) = P (a, c) (b, d)

wrapPair :: a -> Pair a
wrapPair a = P a a

distQuad :: (Quad a, Quad b) -> Quad (a, b)
distQuad (Q a b c d, Q e f g h) = Q (a, e) (b, f) (c, g) (d, h)

wrapQuad :: a -> Quad a
wrapQuad a = Q a a a a

distAnnotated :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated (a :# e1, b :# e2) = (a, b) :# (e1 <> e2)

wrapAnnotated :: Monoid e => a -> Annotated e a
wrapAnnotated a = a :# mempty

distExcept :: (Except e a, Except e b) -> Except e (a, b)
distExcept (Error e, _)           = Error e
distExcept (_, Error e)           = Error e
distExcept (Success a, Success b) = Success (a, b)

wrapExcept :: a -> Except e a
wrapExcept a = Success a

-- distPrioritised must pick the higher priority out of the two.
distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised (High a, High b)     = High (a, b)
distPrioritised (High a, Medium b)   = High (a, b)
distPrioritised (High a, Low b)      = High (a, b)
distPrioritised (Medium a, High b)   = High (a, b)
distPrioritised (Medium a, Medium b) = Medium (a, b)
distPrioritised (Medium a, Low b)    = Medium (a, b)
distPrioritised (Low a, High b)      = High (a, b)
distPrioritised (Low a, Medium b)    = Medium (a, b)
distPrioritised (Low a, Low b)       = Low (a, b)

-- distPrioritised (wrapPrioritised a, wrapPrioritised b)  ≅  wrapPrioritised (a, b)
-- don't requires anything special

-- distPrioritised (p, distPrioritised (q, r))   ≅  distPrioritised (distPrioritised (p, q), r)
-- p = low, q = medium, r = high
-- dist (low, dist (medium, high)) = dist (dist (low, medium), high)
-- dist (low, high) = dist (medium, high)
-- seems ok

-- distPrioritised (wrapPrioritised (), q)  ≅  q
-- let q = Medium
-- dist (?, Medium) = Medium
-- so wrap should be low!!!1

wrapPrioritised :: a -> Prioritised a
wrapPrioritised a = Low a

distStream :: (Stream a, Stream b) -> Stream (a, b)
distStream (a :> b, c :> d) = (a, c) :> distStream (b, d)

wrapStream :: a -> Stream a
wrapStream a = a :> (wrapStream a)

-- distList must associate each element of the first list with each element
-- of the second list (i.e. the resulting list is of length n × m).
distList :: (List a, List b) -> List (a, b)
distList (Nil, _) = Nil
distList (_, Nil) = Nil
distList (x :. xs, y :. ys) =
  (x, y) :. do_magic ys
  where
    do_magic list = case list of
      Nil       -> distList (xs, y :. ys)
      (z :. zs) -> (x, z) :. do_magic zs

wrapList :: a -> List a
wrapList a = a :. Nil

distFun :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun (F a, F b) = F $ \i -> (a i, b i)

wrapFun :: a -> Fun i a
wrapFun a = F $ \_ -> a
