{-# LANGUAGE InstanceSigs #-}

module HW2.T4
  ( State (..),
    mapState,
    wrapState,
    joinState,
    modifyState,
    Prim (..),
    Expr (..),
    eval,
  )
where

import qualified Control.Monad
import HW2.T1

-- define the following data type

data State s a = S {runS :: s -> Annotated s a}

-- (runS s) is (Annotated s a)
-- data Annotated e a = a :# e

mapState :: (a -> b) -> State s a -> State s b
mapState f (S s) = S $ \s' -> mapAnnotated f (s s')

wrapState :: a -> State s a
wrapState a = S $ \s -> a :# s

-- if (S s), then
-- s is (s -> Annotated s (State s a)) or
-- s is (s -> (State s a) :# e) or
-- s is (s -> (s -> a :# e1) :# e2)

joinState :: State s (State s a) -> State s a
joinState (S s) = S $ \s' ->
  let ((S s2) :# s2') = s s'
   in s2 s2'

-- State s () is (s -> Annotated s ()) or
-- is (s -> () :# s)

modifyState :: (s -> s) -> State s ()
modifyState f = S $ \s' -> () :# f s'

-- Using those functions, define Functor,
-- Applicative, and Monad instances:

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap = mapState

instance Applicative (State s) where
  pure :: a -> State s a
  pure = wrapState
  (<*>) :: State s (a -> b) -> State s a -> State s b
  p <*> q = Control.Monad.ap p q

instance Monad (State s) where
  (>>=) :: State s a -> (a -> State s b) -> State s b
  m >>= f = joinState (fmap f m)

-- These instances will enable the use of do-notation with State.
-- The semantics of State are such that the following holds:

-- runS (do
--    modifyState f;
--    modifyState g;
--    return a) x
-- ≡ a :# g (f x)

-- ok, let's say.

-- Define the following data type, representing a small language:

data Prim a
  = Add a a -- (+)
  | Sub a a -- (-)
  | Mul a a -- (*)
  | Div a a -- (/)
  | Abs a -- abs
  | Sgn a -- signum

data Expr = Val Double | Op (Prim Expr)

instance Num Expr where
  (+) :: Expr -> Expr -> Expr
  x + y = Op (Add x y)

  (-) :: Expr -> Expr -> Expr
  x - y = Op (Sub x y)

  (*) :: Expr -> Expr -> Expr
  x * y = Op (Mul x y)

  abs :: Expr -> Expr
  abs x = Op (Abs x)

  signum :: Expr -> Expr
  signum x = Op (Sgn x)

  fromInteger :: Integer -> Expr
  fromInteger x = Val (fromInteger x)

instance Fractional Expr where
  (/) :: Expr -> Expr -> Expr
  x / y = Op (Div x y)

  fromRational :: Rational -> Expr
  fromRational x = Val (fromRational x)

-- Using do-notation for State and combinators
-- we defined for it (pure, modifyState), define
-- the evaluation function.

-- In addition to the final result of evaluating
-- an expression, it accumulates a trace of all
-- individual operations:

-- runS (eval (2 + 3 * 5 - 7)) []
-- ≡ 10 :# [Sub 17 7, Add 2 15, Mul 3 5]

evalBinaryOp ::
  (Double -> Double -> (Double, Prim Double)) ->
  Expr ->
  Expr ->
  ((Double, Prim Double) -> State [Prim Double] Double) ->
  State [Prim Double] Double
evalBinaryOp f x y end = do
  a <- eval x
  b <- eval y
  end $ f a b

evalUnaryOp ::
  (Double -> (Double, Prim Double)) ->
  Expr ->
  ((Double, Prim Double) -> State [Prim Double] Double) ->
  State [Prim Double] Double
evalUnaryOp f x end = do
  a <- eval x
  end $ f a

evalStateModifier :: (Double, Prim Double) -> State [Prim Double] Double
evalStateModifier (result, op) = do
  modifyState (op :)
  return result

eval :: Expr -> State [Prim Double] Double
eval (Val double)   = do return double
eval (Op (Add x y)) = evalBinaryOp (\a b -> (a + b, Add a b)) x y evalStateModifier
eval (Op (Sub x y)) = evalBinaryOp (\a b -> (a - b, Sub a b)) x y evalStateModifier
eval (Op (Mul x y)) = evalBinaryOp (\a b -> (a * b, Mul a b)) x y evalStateModifier
eval (Op (Div x y)) = evalBinaryOp (\a b -> (a / b, Div a b)) x y evalStateModifier
eval (Op (Abs x))   = evalUnaryOp (\a -> (abs a, Abs a)) x evalStateModifier
eval (Op (Sgn x))   = evalUnaryOp (\a -> (signum a, Sgn a)) x evalStateModifier
