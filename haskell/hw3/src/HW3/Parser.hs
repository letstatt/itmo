{-# LANGUAGE TypeApplications #-}

module HW3.Parser
  (
    parse
  )
where

import Control.Applicative (liftA2)
import Control.Monad.Combinators.Expr (Operator (InfixL, InfixN, InfixR), makeExprParser)
import Data.Char (isAlpha, isAlphaNum)
import Data.List (intercalate)
import qualified Data.Text as T
import Data.Void (Void)
import HW3.Base (HiAction (..), HiExpr (..), HiFun (..), HiValue (..))
import Text.Megaparsec (MonadParsec (eof, notFollowedBy), ParseErrorBundle, Parsec, choice, empty,
                        many, manyTill, notFollowedBy, runParser, satisfy, sepBy, sepBy1, sepEndBy,
                        try, (<|>))
import Text.Megaparsec.Char (char, hexDigitChar, space, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

-- Parser routines

sc :: Parser ()
sc = L.space
  space1
  empty
  empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

hex :: Parser HiExpr
hex = do
  a <- hexDigitChar
  b <- hexDigitChar
  let c = read ("0x" <> [a, b]) -- convert hex to integer
  pure . HiExprValue . HiValueNumber . toRational @Integer $ c

-- Parser rules

pValueNumber :: Parser HiValue
pValueNumber = lexeme $ HiValueNumber . toRational <$> L.signed space L.scientific

pValueFunction :: Parser HiValue
pValueFunction = lexeme $ HiValueFunction <$> choice
  [ HiFunDiv <$ string "div"
  , HiFunMul <$ string "mul"
  , HiFunAdd <$ string "add"
  , HiFunSub <$ string "sub"
  , HiFunAnd <$ string "and"
  , HiFunOr <$ string "or"
  , HiFunLessThan <$ string "less-than"
  , HiFunGreaterThan <$ string "greater-than"
  , HiFunEquals <$ string "equals"
  , HiFunNotLessThan <$ string "not-less-than"
  , HiFunNotGreaterThan <$ string "not-greater-than"
  , HiFunNotEquals <$ string "not-equals"
  , HiFunIf <$ string "if"
  , HiFunNot <$ string "not" -- put "not" lower because of greedy tokenizing
  , HiFunLength <$ string "length"
  , HiFunToUpper <$ string "to-upper"
  , HiFunToLower <$ string "to-lower"
  , HiFunReverse <$ string "reverse"
  , HiFunTrim <$ string "trim"
  , HiFunList <$ string "list"
  , HiFunRange <$ string "range"
  , HiFunFold <$ string "fold"
  , HiFunPackBytes <$ string "pack-bytes"
  , HiFunUnpackBytes <$ string "unpack-bytes"
  , HiFunZip <$ string "zip"
  , HiFunUnzip <$ string "unzip"
  , HiFunEncodeUtf8 <$ string "encode-utf8"
  , HiFunDecodeUtf8 <$ string "decode-utf8"
  , HiFunSerialise <$ string "serialise"
  , HiFunDeserialise <$ string "deserialise"
  , HiFunRead <$ string "read"
  , HiFunWrite <$ string "write"
  , HiFunMkDir <$ string "mkdir"
  , HiFunChDir <$ string "cd"
  , HiFunParseTime <$ string "parse-time"
  , HiFunRand <$ string "rand"
  , HiFunEcho <$ string "echo"
  , HiFunCount <$ string "count"
  , HiFunKeys <$ string "keys"
  , HiFunValues <$ string "values"
  , HiFunInvert <$ string "invert"
  ]

pValueBool :: Parser HiValue
pValueBool = lexeme $ HiValueBool <$> choice
  [ True <$ string "true"
  , False <$ string "false"
  ]

pValueNull :: Parser HiValue
pValueNull = lexeme $ HiValueNull <$ string "null"

pValueString :: Parser HiValue
pValueString = lexeme $ HiValueString . T.pack <$> (
  char '\"' *> manyTill L.charLiteral (char '\"')
  )

pValueAction :: Parser HiValue
pValueAction = lexeme $ HiValueAction <$> choice
  [ HiActionCwd <$ string "cwd"
  , HiActionNow <$ string "now"
  ]

pValue :: Parser HiValue
pValue = choice [
  pValueNumber,
  pValueFunction,
  pValueBool,
  pValueNull,
  pValueString,
  pValueAction
  ]

pValuesBytesSugar :: Parser HiExpr
pValuesBytesSugar = (\b -> HiExprApply f [g b]) <$> pEnclosed "[#" (lexeme $ sepEndBy hex space1) "#]"
  where
    f = HiExprValue $ HiValueFunction HiFunPackBytes
    g = HiExprApply $ HiExprValue $ HiValueFunction HiFunList

pValueListSugar :: Parser HiExpr
pValueListSugar = HiExprApply f <$> pExprPack "[" "]"
  where
    f = HiExprValue $ HiValueFunction HiFunList

pValueDictSugar :: Parser HiExpr
pValueDictSugar = HiExprDict <$> pEnclosed "{" p "}"
  where
    p = lexeme $ liftA2 (,) (pExpr <* symbol ":") pExpr `sepBy` symbol ","

pValueDesugar :: Parser HiExpr
pValueDesugar = choice
  [ HiExprValue <$> pValue
  , symbol "(" *> pExpr <* symbol ")"
  , pValuesBytesSugar
  , pValueListSugar
  , pValueDictSugar
  ]

pEnclosed :: String -> Parser [a] -> String -> Parser [a]
pEnclosed left middle right = do
  _ <- lexeme $ symbol left
  args <- middle
  _ <- lexeme $ symbol right
  return args

pExprPack :: String -> String -> Parser [HiExpr]
pExprPack l = pEnclosed l (pExpr `sepBy` symbol ",")

pDotAccess :: Parser HiExpr
pDotAccess = do
  _ <- symbol "."
  HiExprValue . HiValueString . T.pack <$> (intercalate "-" <$> lexeme pId)
  where
    pId = liftA2 (:) (satisfy isAlpha) (many (satisfy isAlphaNum)) `sepBy1` char '-'

pExprApplication :: HiExpr -> Parser HiExpr
pExprApplication expr = choice
  [ HiExprApply expr <$> pExprPack "(" ")"
  , HiExprRun expr <$ symbol "!"
  , (\x -> HiExprApply expr [x]) <$> pDotAccess
  ]

pTerm :: Parser HiExpr
pTerm = do
  let val = pValueDesugar -- read value
  appReduce =<< val       -- reduce application (e.g. (...), !, .ident)
  where
    appReduce :: HiExpr -> Parser HiExpr
    appReduce expr = do
      let ap = pExprApplication expr
      try (appReduce =<< ap) <|> pure expr

pExpr :: Parser HiExpr
pExpr = makeExprParser pTerm operatorsTable

operatorsTable :: [[Operator Parser HiExpr]]
operatorsTable = [
  [ binary InfixL (string "=") "/" HiFunDiv -- highest precedence
  , binaryL "*" HiFunMul],

  [ binaryL "+" HiFunAdd
  , binaryL "-" HiFunSub],

  [ binaryN "<=" HiFunNotGreaterThan
  , binaryN "<" HiFunLessThan
  , binaryN ">=" HiFunNotLessThan
  , binaryN ">" HiFunGreaterThan
  , binaryN "/=" HiFunNotEquals
  , binaryN "==" HiFunEquals],

  [ binaryR "&&" HiFunAnd ],

  [ binaryR "||" HiFunOr ] -- lowest precedence
  ]
  where
    binaryL = binary InfixL empty
    binaryN = binary InfixN empty
    binaryR = binary InfixR empty
    application f a b =
      HiExprApply (HiExprValue . HiValueFunction $ f) [a, b]
    binary assoc discard name f =
      assoc $ application f <$ (lexeme . try) (symbol name <* notFollowedBy discard)

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (space *> pExpr <* eof) ""
