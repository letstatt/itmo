{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}

module HW2.T6
  ( ParseError (..),
    Parser (..),
    runP,
    pChar,
    parseError,
    pEof,
    skipSpaces,
    expect,
    expect2,
    parseInteger,
    toNumber,
    parseN,
    parseE,
    parseT,
    parseE',
    parseT',
    parseF,
    parseExpr,
  )
where

import Control.Applicative (Alternative, optional)
import Control.Monad (MonadPlus, mfilter)
import Data.Char (digitToInt, isDigit, isSpace)
import GHC.Base (Alternative (..))
import GHC.Float (rationalToDouble)
import GHC.Natural (Natural)
import HW2.T1 (Annotated (..), Except (..))
import HW2.T4 (Expr (..), Prim (..))
import HW2.T5 (ExceptState (..))

data ParseError = ErrorAtPos Natural

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

-- Our state consists of a Natural representing
-- how many characters we have already consumed
-- (for error messages) and the String is the
-- remainder of the input.

runP :: Parser a -> String -> Except ParseError a
runP (P (ES es)) s =
  case es (0, s) of
    Error e    -> Error e
    Success s' -> let (a :# _) = s' in Success a

-- If the string is empty, Error (ErrorAtPos pos) occurs.
-- If not, after a char consumed we get that character,
-- annotated with its position and the rest of the input string.
pChar :: Parser Char
pChar = P $
  ES $ \(pos, s) ->
    case s of
      []       -> Error (ErrorAtPos pos)
      (c : cs) -> Success (c :# (pos + 1, cs))

parseError :: Parser a
parseError = P $ ES $ \(pos, _) -> Error (ErrorAtPos pos)

instance Alternative Parser where
  empty :: Parser a
  empty = parseError

  (<|>) :: Parser a -> Parser a -> Parser a
  (P (ES p)) <|> (P (ES q)) = P $
    ES $ \arg -> case p arg of
      Error _ -> q arg
      success -> success

instance MonadPlus Parser

pEof :: Parser ()
pEof = P $
  ES $ \arg@(pos, s) -> case s of
    [] -> Success (() :# arg)
    _  -> Error (ErrorAtPos pos)

-- let's remember translation methods course
-- since we need to parse the string consisting of
-- arithmetical operators with proper precedence.

-- parseExpr :: String -> Except ParseError Expr

-- we will use context-free grammar and recursive descent
-- because it seems to be easy to implement.
-- E  = TE'
-- E' = +TE'
-- E' = -TE'
-- E' = e
-- T  = FT'
-- T' = *FT'
-- T' = /FT'
-- T' = e
-- F  = n
-- F  = (E)

-- so
-- E  = TE'
-- E' = +TE' or -TE' or e
-- T  = FT'
-- T' = *FT' or /FT' or e
-- F  = n or (E)

-- so
-- E  = TE'
-- E' = (('+' <|> '-') TE' <|> e)
-- T  = FT'
-- T' = (('*' <|> '/') FT' <|> e)
-- F  = n <|> (E)

-- also according to example 3,
-- we need to skip spaces. good.

-- ok, but how to parse numbers?
-- there are integers and doubles.

-- n  = \d+ (\.\d+)?

-- so
-- E  = TE'
-- E' = (('+' <|> '-') TE' <|> e)
-- T  = FT'
-- T' = (('*' <|> '/') FT' <|> e)
-- F  = (some([0-9]) and optional('.' and some([0-9]))) <|> (E)

-- WARNING: since E' and T' contains branches with epsilon,
-- we need to check each calling of them with <|>

skipSpaces :: Parser String
skipSpaces = many (mfilter (\a -> isSpace a) pChar)

expect :: Char -> Parser Char
expect c = mfilter (\c' -> c == c') pChar

expect2 :: Char -> Parser Char
expect2 c = skipSpaces *> mfilter (\c' -> c == c') pChar

parseInteger :: Parser String
parseInteger = some (mfilter isDigit pChar)

toNumber :: String -> Integer
toNumber = foldl (\a b -> a * 10 + toInteger (digitToInt b)) 0

parseN :: Parser Double
parseN = do
  a <- skipSpaces *> parseInteger
  b' <- (optional ((expect '.') *> parseInteger))
  pure $
    ( case b' of
        Just b ->
          let power = (10 ^ length b)
           in rationalToDouble
                ((fromIntegral $ toNumber a) * power + (fromIntegral $ toNumber b))
                power
        Nothing -> (fromIntegral $ toNumber a)
    )

parseE :: Parser Expr
parseE = do
  t <- parseT
  parseE' t <|> pure t -- parse or do not parse E'

parseT :: Parser Expr
parseT = do
  f <- parseF
  parseT' f <|> pure f -- parse or do not parse T'

parseE' :: Expr -> Parser Expr
parseE' operand = do
  op <- expect2 '+' <|> expect2 '-'
  t <- parseT
  res <- case op of
    '+' -> pure $ Op (Add operand t)
    '-' -> pure $ Op (Sub operand t)
    _   -> parseError
  parseE' res <|> pure res -- parse or do not parse E'

parseT' :: Expr -> Parser Expr
parseT' operand = do
  op <- expect2 '*' <|> expect2 '/'
  f <- parseF
  res <- case op of
    '*' -> pure $ Op (Mul operand f)
    '/' -> pure $ Op (Div operand f)
    _   -> parseError
  parseT' res <|> pure res -- parse or do not parse T'

parseF :: Parser Expr
parseF =
  (expect2 '(' *> parseE <* expect2 ')') <|> (fmap Val parseN)

parseExpr :: String -> Except ParseError Expr
parseExpr = runP (parseE <* skipSpaces <* pEof)
