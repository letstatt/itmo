module HW2.T1
  ( Option (..),
    Pair (..),
    Quad (..),
    Annotated (..),
    Except (..),
    Prioritised (..),
    Stream (..),
    List (..),
    Fun (..),
    Tree (..),
    mapOption,
    mapPair,
    mapQuad,
    mapAnnotated,
    mapExcept,
    mapPrioritised,
    mapStream,
    mapList,
    mapFun,
    mapTree,
  )
where

-- Define the following data types in it

data Option a = None | Some a

data Pair a = P a a

data Quad a = Q a a a a

data Annotated e a = a :# e

infix 0 :#

data Except e a = Error e | Success a

data Prioritised a = Low a | Medium a | High a

data Stream a = a :> Stream a

infixr 5 :>

data List a = Nil | a :. List a

infixr 5 :.

data Fun i a = F (i -> a)

data Tree a = Leaf | Branch (Tree a) a (Tree a)

-- For each of those types, implement a function of the following form
-- mapF :: (a -> b) -> (F a -> F b)

-- These functions must modify only the elements and preserve the overall
-- structure (e.g. do not reverse the list, do not rebalance the tree, do not swap the pair)

-- This property is witnessed by the following laws
-- mapF id           ≡  id
-- mapF f ∘ mapF g   ≡  mapF (f ∘ g)

mapOption :: (a -> b) -> (Option a -> Option b)
mapOption f (Some a) = Some $ f a
mapOption _ None     = None

mapPair :: (a -> b) -> (Pair a -> Pair b)
mapPair f (P a b) = P (f a) (f b)

mapQuad :: (a -> b) -> (Quad a -> Quad b)
mapQuad f (Q a b c d) = Q (f a) (f b) (f c) (f d)

mapAnnotated :: (a -> b) -> (Annotated e a -> Annotated e b)
mapAnnotated f (a :# e) = (f a) :# e

mapExcept :: (a -> b) -> (Except e a -> Except e b)
mapExcept _ (Error e)   = Error e
mapExcept f (Success a) = Success $ f a

mapPrioritised :: (a -> b) -> (Prioritised a -> Prioritised b)
mapPrioritised f (Low a)    = Low $ f a
mapPrioritised f (Medium a) = Medium $ f a
mapPrioritised f (High a)   = High $ f a

mapStream :: (a -> b) -> (Stream a -> Stream b)
mapStream f (a :> b) = (f a) :> mapStream f b

mapList :: (a -> b) -> (List a -> List b)
mapList _ Nil      = Nil
mapList f (a :. b) = (f a) :. (mapList f b)

mapFun :: (a -> b) -> (Fun i a -> Fun i b)
mapFun f (F g) = F $ f . g

mapTree :: (a -> b) -> (Tree a -> Tree b)
mapTree _ Leaf             = Leaf
mapTree f (Branch tl y tr) = Branch (mapTree f tl) (f y) (mapTree f tr)
