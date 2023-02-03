{-# LANGUAGE InstanceSigs #-}

module HW1.T7
  ( ListPlus ((:+), Last),
    Inclusive (This, That, Both),
    DotString (DS),
    Fun(F),
  )
where

data ListPlus a = a :+ ListPlus a | Last a
  deriving (Show)

infixr 5 :+

-- semigroup instance
-- <> operator is minimal complete definition

instance Semigroup (ListPlus a) where
  (<>) :: ListPlus a -> ListPlus a -> ListPlus a
  (<>) (Last a) b = a :+ b
  (<>) (a :+ b) c = a :+ (b <> c)

data Inclusive a b = This a | That b | Both a b
  deriving (Show)

-- inclusive instance
-- This i  <>  This j  =  This (i <> j)   -- OK
-- This i  <>  This _  =  This i          -- This is not the Semigroup you're looking for.
-- wtf?
-- a and b are semigroups too?

instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
  (<>) :: Inclusive a b -> Inclusive a b -> Inclusive a b
  (<>) (This x) (This y)      = This (x <> y)
  (<>) (This x) (That y)      = Both x y
  (<>) (This x) (Both y z)    = Both (x <> y) z
  (<>) (That x) (This y)      = Both y x
  (<>) (That x) (That y)      = That (x <> y)
  (<>) (That x) (Both y z)    = Both y $ (<>) x z
  (<>) (Both x y) (This z)    = Both (x <> z) y
  (<>) (Both x y) (That z)    = Both x $ (<>) y z
  (<>) (Both x y) (Both z z1) = Both (x <> z) (y <> z1)

newtype DotString = DS String
  deriving (Show)

-- String == [char]

instance Semigroup DotString where
  (<>) :: DotString -> DotString -> DotString
  (<>) a (DS "")     = a
  (<>) (DS "") a     = a
  (<>) (DS a) (DS b) = DS (a ++ "." ++ b)

-- mempty is minimal complete definition
instance Monoid DotString where
  mempty :: DotString
  mempty = DS ""

newtype Fun a = F (a -> a)

instance Semigroup (Fun a) where
  (<>) :: Fun a -> Fun a -> Fun a
  (<>) (F f) (F g) = F (f . g)

-- ok

instance Monoid (Fun a) where
  mempty :: Fun a
  mempty = F id
