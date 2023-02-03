{-# LANGUAGE TypeApplications #-}

module HW3.Pretty
  (
    prettyValue,
    prettyEvalError,
    prettyParseError
  )
where

import HW3.Base (HiValue(..), HiFun(..), HiError(..), HiAction(..))
import Prettyprinter.Render.Terminal (AnsiStyle, bold, Color(..), color)
import Prettyprinter (Doc, pretty, (<+>), slash, annotate, list, viaShow, encloseSep, space, comma, emptyDoc)
import Data.Scientific (fromRationalRepetendUnlimited, toRealFloat)
import Numeric (showFFloat, showHex)
import Data.Ratio (numerator, denominator)
import Data.Foldable (toList)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty)
import Data.Void (Void)
import Data.Time (UTCTime)
import Data.Map (Map)
import qualified Data.Map as M

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueNumber n) = prettyValueNumber n
prettyValue (HiValueFunction f) = prettyValueFunction f
prettyValue (HiValueBool b) = prettyValueBool b
prettyValue HiValueNull = pretty "null"
prettyValue (HiValueString s) = viaShow s
prettyValue (HiValueList l) = list (prettyValue <$> toList l)
prettyValue (HiValueBytes b) = prettyValueBytes b
prettyValue (HiValueAction a) = prettyValueAction a
prettyValue (HiValueTime t) = prettyValueTime t
prettyValue (HiValueDict m) = prettyValueDict m

prettyEvalError :: HiError -> Doc AnsiStyle
prettyEvalError err = annotate (color Red <> bold) (pretty $ show err)

prettyParseError :: ParseErrorBundle String Void -> Doc AnsiStyle
prettyParseError = annotate (color Red) . pretty . errorBundlePretty

-- pretty printing implementations for each type are below

prettyValueNumber :: Rational -> Doc AnsiStyle
prettyValueNumber a
  | d == 1    = pretty n -- print as is
  | otherwise = prettyFraction a n d -- print as fraction
  where
    n = numerator a
    d = denominator a

-- not sure, but seems that denominator must contain only powers of two
-- and of five to be representable by finite number of digits,
-- except periodical fractions, but idk how to check it.
-- anyway, it isn't needed.
prettyFraction :: Rational -> Integer -> Integer -> Doc AnsiStyle
prettyFraction a num den
  | unmul (unmul den 5) 2 == 1 = prettyFractionFinite a
  | otherwise = let (int, frac) = quotRem num den in
    if int == 0
      then fractionAsIs frac den
      else pretty int <+> pretty (if frac > 0 then '+' else '-') <+> fractionAsIs (abs frac) den
  where
    unmul x y
      | mod x y == 0 = unmul (div x y) y
      | otherwise    = x

    fractionAsIs x y = pretty x <> slash <> pretty y

prettyFractionFinite :: Rational -> Doc AnsiStyle
prettyFractionFinite a = pretty $ showFFloat @Double Nothing repr "" where
  repr = (toRealFloat . fst . fromRationalRepetendUnlimited) a

prettyValueFunction :: HiFun -> Doc AnsiStyle
prettyValueFunction f = pretty (
  case f of 
    HiFunDiv -> "div"
    HiFunMul -> "mul"
    HiFunAdd -> "add"
    HiFunSub -> "sub"
    HiFunNot -> "not"
    HiFunAnd -> "and"
    HiFunOr  -> "or"
    HiFunLessThan -> "less-than"
    HiFunGreaterThan -> "greater-than"
    HiFunEquals -> "equals"
    HiFunNotLessThan -> "not-less-than"
    HiFunNotGreaterThan -> "not-greater-than"
    HiFunNotEquals -> "not-equals"
    HiFunIf -> "if"
    HiFunLength -> "length"
    HiFunToUpper -> "to-upper"
    HiFunToLower -> "to-lower"
    HiFunReverse -> "reverse"
    HiFunTrim -> "trim"
    HiFunList -> "list"
    HiFunRange -> "range"
    HiFunFold -> "fold"
    HiFunPackBytes -> "pack-bytes"
    HiFunUnpackBytes -> "unpack-bytes"
    HiFunZip -> "zip"
    HiFunUnzip -> "unzip"
    HiFunEncodeUtf8 -> "encode-utf8"
    HiFunDecodeUtf8 -> "decode-utf8"
    HiFunSerialise -> "serialise"
    HiFunDeserialise -> "deserialise"
    HiFunRead -> "read"
    HiFunWrite -> "write"
    HiFunMkDir -> "mkdir"
    HiFunChDir -> "cd"
    HiFunParseTime -> "parse-time"
    HiFunRand -> "rand"
    HiFunEcho -> "echo"
    HiFunCount -> "count"
    HiFunKeys -> "keys"
    HiFunValues -> "values"
    HiFunInvert -> "invert"
  )

prettyValueBool :: Bool -> Doc AnsiStyle
prettyValueBool True = pretty "true"
prettyValueBool False = pretty "false"

prettyValueBytes :: ByteString -> Doc AnsiStyle
prettyValueBytes b = l <> foldl (<>) emptyDoc (map hex2doc $ B.unpack b) <> r
  where
    l = pretty "[# "
    r = pretty "#]"
    hex2doc w8 = pretty $ (if w8 < 16 then "0" else "") ++ showHex w8 "" ++ " "

prettyValueAction :: HiAction -> Doc AnsiStyle
prettyValueAction (HiActionRead p) = pretty $ "read(" <> show p <> ")"
prettyValueAction (HiActionWrite p b) = pretty ("read(" <> show p <> ", ") <> prettyValueBytes b <> pretty ")"
prettyValueAction (HiActionMkDir p) = pretty $ "mkdir(" <> show p <> ")"
prettyValueAction (HiActionChDir p) = pretty $ "cd(" <> show p <> ")"
prettyValueAction HiActionCwd = pretty "cwd"
prettyValueAction HiActionNow = pretty "now"
prettyValueAction (HiActionRand l r) = pretty $ "rand(" <> show l <> ", " <> show r <> ")"
prettyValueAction (HiActionEcho s) = pretty $ "echo(" <> show s <> ")"

prettyValueTime :: UTCTime -> Doc AnsiStyle
prettyValueTime t = pretty $ "parse-time(\"" <> show t <> "\")"

prettyValueDict :: Map HiValue HiValue -> Doc AnsiStyle
prettyValueDict m = encloseSep l r (comma <> space) (map kvpretty $ M.assocs m)
    where
      l = pretty "{ "
      r = pretty " }"
      kvpretty (k, v) = prettyValue k <> pretty ": " <> prettyValue v
