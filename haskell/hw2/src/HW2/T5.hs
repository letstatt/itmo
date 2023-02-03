{-# LANGUAGE InstanceSigs #-}

module HW2.T5
  ( ExceptState (..),
    mapExceptState,
    wrapExceptState,
    joinExceptState,
    modifyExceptState,
    throwExceptState,
    EvaluationError (..),
    eval,
  )
where

import qualified Control.Monad
import HW2.T1
import HW2.T4 (Expr (..), Prim (..))

data ExceptState e s a = ES {runES :: s -> Except e (Annotated s a)}

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f (ES se) = ES $ \s ->
  mapExcept (mapAnnotated f) $ se s

wrapExceptState :: a -> ExceptState e s a
wrapExceptState a = ES $ \s ->
  Success $ (a :# s)

-- ExceptState e s (ExceptState e s a) ==
-- s -> Except e (Annotated s (ExceptState e s a)) ==
-- s -> Except e ((ExceptState e s a) :# s') ==
-- s -> Except e ((s2 -> Except e (Annotated s a)) :# s') ==
-- s -> Except e ((s2 -> Except e (a :# s'')) :# s')

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState (ES se) = ES $ \s ->
  let ex = se s
   in case ex of
        Error e                -> Error e
        -- Success (Annotated s (ExceptState e s a))
        Success (ES se' :# s') -> se' s'

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES $ \s ->
  Success (() :# f s)

throwExceptState :: e -> ExceptState e s a
throwExceptState e = ES $ \_ ->
  Error e

-- Using those functions, define Functor,
-- Applicative, and Monad instances.

instance Functor (ExceptState e s) where
  fmap :: (a -> b) -> ExceptState e s a -> ExceptState e s b
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure :: a -> ExceptState e s a
  pure = wrapExceptState

  (<*>) :: ExceptState e s (a -> b) -> ExceptState e s a -> ExceptState e s b
  p <*> q = Control.Monad.ap p q

instance Monad (ExceptState e s) where
  (>>=) :: ExceptState e s a -> (a -> ExceptState e s b) -> ExceptState e s b
  m >>= f = joinExceptState (fmap f m)

data EvaluationError = DivideByZero

-- copypasted, sorry. I didn't want to accidentally break anything.

evalBinaryOp ::
  Monad m => -- constraint
  (Expr -> m Double) -> -- eval function
  (Double -> Double -> (Double, Prim Double)) -> -- calc numbers
  Expr -> -- first arg
  Expr -> -- second arg
  ((Double, Prim Double) -> m Double) -> -- state modifier
  m Double
evalBinaryOp eval' f x y end = do
  a <- eval' x
  b <- eval' y
  end $ f a b

evalUnaryOp ::
  Monad m => -- constraint
  (Expr -> m Double) -> -- eval function
  (Double -> (Double, Prim Double)) -> -- calc numbers
  Expr -> -- first arg
  ((Double, Prim Double) -> m Double) -> -- state modifier
  m Double
evalUnaryOp eval' f x end = do
  a <- eval' x
  end $ f a

evalStateModifier :: (Double, Prim Double) -> ExceptState e [Prim Double] Double
evalStateModifier (result, op) = do
  modifyExceptState (op :)
  return result

-- ExceptState (e=EvaluationError, s=[Prim Double], a=Double)
eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Val double) = do return double
eval (Op (Add x y)) = evalBinaryOp eval (\a b -> (a + b, Add a b)) x y evalStateModifier
eval (Op (Sub x y)) = evalBinaryOp eval (\a b -> (a - b, Sub a b)) x y evalStateModifier
eval (Op (Mul x y)) = evalBinaryOp eval (\a b -> (a * b, Mul a b)) x y evalStateModifier
eval (Op (Abs x)) = evalUnaryOp eval (\a -> (abs a, Abs a)) x evalStateModifier
eval (Op (Sgn x)) = evalUnaryOp eval (\a -> (signum a, Sgn a)) x evalStateModifier
eval (Op (Div x y)) = do
  a <- eval x
  b <- eval y
  if b == 0
    then throwExceptState DivideByZero
    else modifyExceptState (Div a b :)
  return (a / b)
