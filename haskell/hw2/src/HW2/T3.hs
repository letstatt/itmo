module HW2.T3
  ( joinOption,
    joinExcept,
    joinAnnotated,
    joinList,
    joinFun,
  )
where

import HW2.T1

-- For Option, Except, Annotated, List, and Fun define a function of the following form
-- joinF :: F (F a) -> F a

-- The following laws must hold:
-- Associativity:
-- joinF (mapF joinF m)  ≡  joinF (joinF m)

-- In other words, given F (F (F a)), it does not matter
-- whether we join the outer layers or the inner layers first.

-- Left and right identity:
-- joinF      (wrapF m)  ≡  m
-- joinF (mapF wrapF m)  ≡  m

-- In other words, layers created by wrapF are identity elements to joinF.
-- Given F a, you can add layers outside/inside to get F (F a),
-- but joinF flattens it back into F a without any other changes to the structure.

-- Furthermore, joinF is strictly more powerful than distF and can be used to define it:

-- distF (p, q) = joinF (mapF (\a -> mapF (\b -> (a, b)) q) p)
-- At the same time, this is only one of the possible distF definitions
-- (e.g. List admits at least two lawful distF).
-- It is common in Haskell to expect distF and joinF to agree in behavior,
-- so the above equation must hold. (Do not redefine distF using joinF, though:
-- it would be correct but not the point of the exercise).

joinOption :: Option (Option a) -> Option a
joinOption (Some a) = a
joinOption _        = None

joinExcept :: Except e (Except e a) -> Except e a
joinExcept (Success ex) = ex
joinExcept (Error e)    = Error e

-- order? e1 <> e2 or vice versa? hmm

-- joinAnnotated (mapAnnotated joinAnnotated m)
-- ≡ joinAnnotated (joinAnnotated m) ?
-- m is (Annotated e (Annotated e a))
-- let m = ((a :# e1) :# e2)

-- mapAnnotated joinAnnotated m ==
-- (joinAnnotated (a :# e1)) :# e2 ==
-- (a :# e1 <> e3) :# e2
-- joinAnnotated ((a :# e1 <> e3) :# e2) == a :# e1 <> e3 <> e2

-- joinAnnotated ((a :# e1) :# e2) = a :# e1 <> e2
-- joinAnnotated ((a :# e3) :# e1 <> e2) = a:# e3 <> e1 <> e2
-- no. then -> vice versa.

joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((a :# e1) :# e2) = (a :# (e2 <> e1))

-- joinList :: List (List a) -> List a
-- joinList Nil = Nil
-- joinList (a :. b) = undefined
-- there are a = (List a) and b = List (List a) above.
-- so, i want joinList = a ++ (joinList b)
-- but i have no ++. maybe I should construct list from scratch.
-- if a is Nil, then -> ret (joinList b).
-- if a is (x :. Nil) -> ret x :. (joinList b)
-- if a is (x :. xs) -> how? xs is (List a). xs is needed to be unwinded
-- then :. with (joinList b).
-- ok, seems similar to distList.

joinList :: List (List a) -> List a
joinList Nil = Nil
joinList (a :. b) = unwind a
  where
    unwind list = case list of
      Nil       -> joinList b
      (x :. xs) -> x :. unwind xs

-- joinFun :: Fun i (Fun i a) -> Fun i a
-- joinFun (F f) = ?
-- what the hell. how to unwrap function?
-- so f is \i -> (Fun i a)
-- so (f i) is Fun i a
-- oh, ok. let's case it.

joinFun :: Fun i (Fun i a) -> Fun i a
joinFun (F f) = F $ \i ->
  ( case f i of
      (F g) -> g
  )
    i

-- or better to use "where", maybe.
