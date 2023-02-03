{-# LANGUAGE TypeApplications #-}

module HW3.Evaluator
  (
    eval
  )
where

import Codec.Compression.Zlib (CompressParams (..), bestCompression, compressWith, decompress,
                               defaultCompressParams)
import Codec.Serialise (deserialiseOrFail, serialise)
import Control.Applicative (liftA2)
import Control.Monad (join)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import qualified Data.ByteString as B
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Foldable (foldl', toList)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Map.Strict as M.Strict
import Data.Maybe (fromMaybe)
import Data.Ratio (denominator, numerator)
import Data.Semigroup (stimes)
import Data.Sequence (Seq (Empty, (:<|)), empty, fromList, (<|))
import qualified Data.Sequence as S
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time (UTCTime, addUTCTime, diffUTCTime)
import HW3.Base (HiAction (..), HiError (..), HiExpr (..), HiFun (..), HiMonad (runAction),
                 HiValue (..))
import Text.Read (readMaybe)

data HiArity
  = HiArityUnary
  | HiArityBinary
  | HiArityTernary
  | HiArityVariadic
  deriving Eq

numToArity :: (Num a, Eq a, Monad m) => a -> ExceptT HiError m HiArity
numToArity 1 = pure HiArityUnary
numToArity 2 = pure HiArityBinary
numToArity 3 = pure HiArityTernary
numToArity _ = throwE HiErrorArityMismatch

funToArity :: HiFun -> HiArity
funToArity HiFunDiv            = HiArityBinary
funToArity HiFunMul            = HiArityBinary
funToArity HiFunAdd            = HiArityBinary
funToArity HiFunSub            = HiArityBinary
funToArity HiFunNot            = HiArityUnary
funToArity HiFunAnd            = HiArityBinary
funToArity HiFunOr             = HiArityBinary
funToArity HiFunLessThan       = HiArityBinary
funToArity HiFunGreaterThan    = HiArityBinary
funToArity HiFunEquals         = HiArityBinary
funToArity HiFunNotLessThan    = HiArityBinary
funToArity HiFunNotGreaterThan = HiArityBinary
funToArity HiFunNotEquals      = HiArityBinary
funToArity HiFunIf             = HiArityTernary
funToArity HiFunLength         = HiArityUnary
funToArity HiFunToUpper        = HiArityUnary
funToArity HiFunToLower        = HiArityUnary
funToArity HiFunReverse        = HiArityUnary
funToArity HiFunTrim           = HiArityUnary
funToArity HiFunList           = HiArityVariadic
funToArity HiFunRange          = HiArityBinary
funToArity HiFunFold           = HiArityBinary
funToArity HiFunPackBytes      = HiArityUnary
funToArity HiFunUnpackBytes    = HiArityUnary
funToArity HiFunEncodeUtf8     = HiArityUnary
funToArity HiFunDecodeUtf8     = HiArityUnary
funToArity HiFunZip            = HiArityUnary
funToArity HiFunUnzip          = HiArityUnary
funToArity HiFunSerialise      = HiArityUnary
funToArity HiFunDeserialise    = HiArityUnary
funToArity HiFunRead           = HiArityUnary
funToArity HiFunWrite          = HiArityBinary
funToArity HiFunMkDir          = HiArityUnary
funToArity HiFunChDir          = HiArityUnary
funToArity HiFunParseTime      = HiArityUnary
funToArity HiFunRand           = HiArityBinary
funToArity HiFunEcho           = HiArityUnary
funToArity HiFunCount          = HiArityUnary
funToArity HiFunKeys           = HiArityUnary
funToArity HiFunValues         = HiArityUnary
funToArity HiFunInvert         = HiArityUnary

unaryFunExec :: HiMonad m => HiFun -> HiValue -> ExceptT HiError m HiValue
unaryFunExec HiFunNot (HiValueBool a) =
  (pure . HiValueBool . not) a
unaryFunExec HiFunLength (HiValueString s) =
  (pure . HiValueNumber . toRational . T.length) s
unaryFunExec HiFunLength (HiValueList l) =
  (pure . HiValueNumber . toRational . S.length) l
unaryFunExec HiFunToUpper (HiValueString s) =
  (pure . HiValueString . T.toUpper) s
unaryFunExec HiFunToLower (HiValueString s) =
  (pure . HiValueString . T.toLower) s
unaryFunExec HiFunReverse (HiValueString s) =
  (pure . HiValueString . T.reverse) s
unaryFunExec HiFunReverse (HiValueList l) =
  (pure . HiValueList . S.reverse) l
unaryFunExec HiFunReverse (HiValueBytes b) =
  (pure . HiValueBytes . B.reverse) b
unaryFunExec HiFunTrim (HiValueString s) =
  (pure . HiValueString . T.strip) s
unaryFunExec HiFunPackBytes (HiValueList l) =
    HiValueBytes . B.pack <$> mapM helper (toList l)
      where
        helper (HiValueNumber n) = do
          i <- rationalToInt n
          if 0 <= i && i <= 255
            then pure . fromInteger . toInteger $ i
            else throwE HiErrorInvalidArgument
        helper _ = throwE HiErrorInvalidArgument
unaryFunExec HiFunUnpackBytes (HiValueBytes b) =
  (pure . HiValueList . S.fromList . map (HiValueNumber . toRational) . B.unpack) b
unaryFunExec HiFunEncodeUtf8 (HiValueString s) =
  (pure . HiValueBytes . encodeUtf8) s
unaryFunExec HiFunDecodeUtf8 (HiValueBytes b) =
  (pure . either (const HiValueNull) HiValueString . decodeUtf8') b
unaryFunExec HiFunZip (HiValueBytes b) =
  (pure . HiValueBytes . toStrict . compressWith defaultCompressParams { compressLevel = bestCompression } . fromStrict) b
unaryFunExec HiFunUnzip (HiValueBytes b) =
  (pure . HiValueBytes . toStrict . decompress . fromStrict) b
unaryFunExec HiFunSerialise value =
  (pure . HiValueBytes . toStrict . serialise) value
unaryFunExec HiFunDeserialise (HiValueBytes b) =
  either (const $ throwE HiErrorInvalidArgument) pure $ deserialiseOrFail $ fromStrict b
unaryFunExec HiFunRead (HiValueString s) =
  (pure . HiValueAction . HiActionRead . T.unpack) s
unaryFunExec HiFunMkDir (HiValueString s) =
  (pure . HiValueAction . HiActionMkDir . T.unpack) s
unaryFunExec HiFunChDir (HiValueString s) =
  (pure . HiValueAction . HiActionChDir . T.unpack) s
unaryFunExec HiFunParseTime (HiValueString s) =
  pure $ maybe HiValueNull HiValueTime (readMaybe @UTCTime $ T.unpack s)
unaryFunExec HiFunEcho (HiValueString s) =
  (pure . HiValueAction . HiActionEcho) s
unaryFunExec HiFunCount value =
  HiValueDict . getCount <$> asContainer value
unaryFunExec HiFunKeys (HiValueDict m) =
  (pure . HiValueList . S.fromList . M.keys) m
unaryFunExec HiFunValues (HiValueDict m) =
  (pure . HiValueList . S.fromList . M.elems) m
unaryFunExec HiFunInvert (HiValueDict m) =
  pure $ HiValueDict $ M.map (HiValueList . S.fromList) $ M.foldlWithKey' alter M.empty m
    where
      alter m' k v = M.Strict.alter (maybe (Just [k]) (Just . (k :))) v m'
unaryFunExec _ _ = throwE HiErrorInvalidArgument

binaryFunExec :: HiMonad m => HiFun -> HiValue -> HiValue -> ExceptT HiError m HiValue
binaryFunExec HiFunDiv (HiValueNumber _) (HiValueNumber 0) =
  throwE HiErrorDivideByZero
binaryFunExec HiFunDiv (HiValueNumber a) (HiValueNumber b) =
  pure $ HiValueNumber (a / b)
binaryFunExec HiFunDiv (HiValueString a) (HiValueString b) =
  pure $ HiValueString $ a <> T.pack "/" <> b
binaryFunExec HiFunMul (HiValueNumber a) (HiValueNumber b) =
  pure $ HiValueNumber (a * b)
binaryFunExec HiFunMul (HiValueString a) (HiValueNumber b) =
  do
    i <- rationalToInt b
    if i > 0
      then pure $ HiValueString (stimes i a)
      else throwE HiErrorInvalidArgument
binaryFunExec HiFunMul (HiValueList a) (HiValueNumber b) =
  do
    i <- rationalToInt b
    if i > 0
      then pure $ HiValueList (stimes i a)
      else throwE HiErrorInvalidArgument
binaryFunExec HiFunMul (HiValueBytes a) (HiValueNumber b) =
  do
    i <- rationalToInt b
    if i > 0
      then pure $ HiValueBytes (stimes i a)
      else throwE HiErrorInvalidArgument
binaryFunExec HiFunAdd (HiValueNumber a) (HiValueNumber b) =
  pure $ HiValueNumber (a + b)
binaryFunExec HiFunAdd (HiValueString a) (HiValueString b) =
  pure $ HiValueString $ T.concat [a, b]
binaryFunExec HiFunAdd (HiValueList a) (HiValueList b) =
  pure $ HiValueList $ a S.>< b
binaryFunExec HiFunAdd (HiValueBytes a) (HiValueBytes b) =
  pure $ HiValueBytes $ a <> b
binaryFunExec HiFunAdd (HiValueTime a) (HiValueNumber b) =
  pure $ HiValueTime $ addUTCTime (fromRational b) a
binaryFunExec HiFunSub (HiValueNumber a) (HiValueNumber b) =
  pure $ HiValueNumber (a - b)
binaryFunExec HiFunSub (HiValueTime a) (HiValueTime b) =
  pure $ HiValueNumber $ toRational $ diffUTCTime a b
binaryFunExec HiFunLessThan a b =
  pure . HiValueBool $ case (a, b) of
    (HiValueBool False, HiValueBool True)    -> True
    (HiValueBool _, HiValueNumber _)         -> True
    (HiValueNumber n1, HiValueNumber n2)     -> n1 < n2
    (HiValueString s1, HiValueString s2)     -> s1 < s2 -- is it needed?
    (HiValueFunction f1, HiValueFunction f2) -> f1 < f2
    _                                        -> False
binaryFunExec HiFunGreaterThan a b =
  binaryFunExec HiFunLessThan b a
binaryFunExec HiFunEquals a b =
  pure $ HiValueBool $ case (a, b) of
    (HiValueBool b1, HiValueBool b2)         -> b1 == b2
    (HiValueFunction f1, HiValueFunction f2) -> f1 == f2
    (HiValueNumber n1, HiValueNumber n2)     -> n1 == n2
    (HiValueNull, HiValueNull)               -> True
    (HiValueString s1, HiValueString s2)     -> s1 == s2
    _                                        -> False
binaryFunExec HiFunNotLessThan a b =
  unaryFunExec HiFunNot =<< binaryFunExec HiFunLessThan a b
binaryFunExec HiFunNotGreaterThan a b =
   unaryFunExec HiFunNot =<< binaryFunExec HiFunGreaterThan a b
binaryFunExec HiFunNotEquals a b =
  unaryFunExec HiFunNot =<< binaryFunExec HiFunEquals a b
binaryFunExec HiFunRange (HiValueNumber a) (HiValueNumber b) =
  pure . HiValueList $ range a b
    where
      range x y
        | x <= y + 0.5 = HiValueNumber x <| range (x + 1) y
        | otherwise    = empty
binaryFunExec HiFunFold (HiValueFunction f) (HiValueList l) = -- note that and/or are not in binaryFunExec
  let fun i j = tryShortCircuit HiArityBinary f [HiExprValue i, HiExprValue j] in
    case l of
      Empty      -> throwE HiErrorInvalidArgument
      (x :<| xs) -> foldl' (\a b -> join $ liftA2 fun a (pure b)) (pure x) xs
binaryFunExec HiFunWrite (HiValueString p) (HiValueString s) =
  pure . HiValueAction $ HiActionWrite (T.unpack p) (encodeUtf8 s)
binaryFunExec HiFunRand (HiValueNumber a) (HiValueNumber b) =
  liftA2 (\l r -> HiValueAction $ HiActionRand l r) (rationalToInt a) (rationalToInt b)
binaryFunExec _ _ _ = throwE HiErrorInvalidArgument

variadicFunExec :: HiMonad m => HiFun -> [HiValue] -> ExceptT HiError m HiValue
variadicFunExec HiFunList args =
  (pure . HiValueList . fromList) args
variadicFunExec _ _ = throwE HiErrorInvalidArgument

tryShortCircuit :: HiMonad m => HiArity -> HiFun -> [HiExpr] -> ExceptT HiError m HiValue
tryShortCircuit HiArityTernary HiFunIf [a, b, c] = do
  cond <- evalT a
  case cond of
    HiValueBool True  -> evalT b
    HiValueBool False -> evalT c
    _                 -> throwE HiErrorInvalidArgument
tryShortCircuit HiArityBinary HiFunAnd [a, b] = do
  x <- evalT a
  case x of
    HiValueBool False -> pure x
    HiValueNull       -> pure x
    _                 -> evalT b
tryShortCircuit HiArityBinary HiFunOr [a, b] = do
  x <- evalT a
  case x of
    HiValueBool False -> evalT b
    HiValueNull       -> evalT b
    _                 -> pure x
tryShortCircuit arity f args = do
  args2 <- mapM evalT args
  case arity of
    HiArityUnary    -> unaryFunExec f $ head args2
    HiArityBinary   -> binaryFunExec f (head args2) (args2 !! 1)
    HiArityVariadic -> variadicFunExec f args2
    _               -> throwE HiErrorArityMismatch

exprApplier :: HiMonad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
exprApplier f args = do
  let actualArity = funToArity f

  arityIsGood <- do
    if actualArity == HiArityVariadic
      then pure True
      else (== actualArity) <$> numToArity (length args)

  if not arityIsGood
    then throwE HiErrorArityMismatch
    else tryShortCircuit actualArity f args

rationalToInt :: HiMonad m => Rational -> ExceptT HiError m Int
rationalToInt n
  | denominator n /= 1 = throwE HiErrorInvalidArgument
  | otherwise          = pure . fromIntegral . numerator $ n

data Container = Container
  { getThis   :: HiValue
  , getLength :: Int
  , getIndex  :: Int -> HiValue
  , getTake   :: Int -> HiValue
  , getDrop   :: Int -> HiValue
  , getCount  :: Map HiValue HiValue} -- welcome to lazy world

asContainer :: HiMonad m => HiValue -> ExceptT HiError m Container
asContainer this@(HiValueString s) = pure $ Container
  { getThis = this
  , getLength = T.length s
  , getIndex = HiValueString . T.singleton . T.index s
  , getTake = \i -> HiValueString $ T.take i s
  , getDrop = \i -> HiValueString $ T.drop i s
  , getCount = containerCount (\f a -> T.foldl' f a s) (HiValueString . T.singleton)}
asContainer this@(HiValueList l) = pure $ Container
  { getThis = this
  , getLength = S.length l
  , getIndex = S.index l
  , getTake = \i -> HiValueList $ S.take i l
  , getDrop = \i -> HiValueList $ S.drop i l
  , getCount = containerCount (\f a -> foldl' f a l) id}
asContainer this@(HiValueBytes b) = pure $ Container
  { getThis = this
  , getLength = B.length b
  , getIndex = HiValueNumber . toRational . B.index b
  , getTake = \i -> HiValueBytes $ B.take i b
  , getDrop = \i -> HiValueBytes $ B.drop i b
  , getCount = containerCount (\f a -> B.foldl' f a b) (HiValueNumber . toRational)}
asContainer _ = throwE HiErrorInvalidFunction -- cap for evalT

containerCount :: ((Map HiValue Rational -> v -> Map HiValue Rational) -> Map HiValue Rational -> Map HiValue Rational) -> (v -> HiValue) -> Map HiValue HiValue
containerCount fold' singleton = M.map HiValueNumber $ fold' alter M.empty
  where
    alter m' i = M.Strict.alter (maybe (Just 1) (Just . (+1))) (singleton i) m'

containerTakeByIndex :: HiMonad m => Container -> HiValue -> ExceptT HiError m HiValue
containerTakeByIndex vtable (HiValueNumber n) = do
  i <- rationalToInt n
  pure $ if i >= 0 && i < getLength vtable
    then getIndex vtable i
    else HiValueNull
containerTakeByIndex _ _ = throwE HiErrorInvalidArgument

containerTakeSlice :: HiMonad m => Container -> HiValue -> HiValue -> ExceptT HiError m HiValue
containerTakeSlice vtable HiValueNull HiValueNull = pure $ getThis vtable
containerTakeSlice vtable HiValueNull (HiValueNumber n) = containerPrefix vtable <$> rationalToInt n
containerTakeSlice vtable (HiValueNumber n) HiValueNull = containerSuffix vtable <$> rationalToInt n
containerTakeSlice vtable (HiValueNumber n) (HiValueNumber m) = do
  part <- containerSuffix vtable <$> rationalToInt n
  vtable2 <- asContainer part
  containerPrefix vtable2 <$> rationalToInt (if m <= 0 then m else m - n)
containerTakeSlice _ _ _ = throwE HiErrorInvalidArgument

containerPrefix :: Container -> Int -> HiValue
containerPrefix vtable = containerCut (getTake vtable) (getLength vtable)

containerSuffix :: Container -> Int -> HiValue
containerSuffix vtable = containerCut (getDrop vtable) (getLength vtable)

containerCut :: (Int -> HiValue) -> Int -> Int -> HiValue
containerCut cutter len i = if i < 0 then cutter (len + i) else cutter i

containerExprApplier :: HiMonad m => Container -> [HiExpr] -> ExceptT HiError m HiValue
containerExprApplier vtable args = do
  arity <- numToArity $ length args
  case arity of
    HiArityUnary -> do
      args2 <- mapM evalT args
      containerTakeByIndex vtable (head args2)
    HiArityBinary -> do
        args2 <- mapM evalT args
        containerTakeSlice vtable (head args2) (args2 !! 1)
    _ -> throwE HiErrorArityMismatch

evalT :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
evalT (HiExprValue value) = pure value
evalT (HiExprApply expr args) = do
  val <- evalT expr
  case val of
    HiValueFunction f -> exprApplier f args
    HiValueDict d -> do
      if length args /= 1
        then throwE HiErrorArityMismatch
        else extractor <$> evalT (head args)
          where
            extractor k = fromMaybe HiValueNull $ M.lookup k d
    _ -> (`containerExprApplier` args) =<< asContainer val
evalT (HiExprRun expr) = do
  val <- evalT expr
  case val of
    HiValueAction a -> lift $ runAction a
    _               -> throwE HiErrorInvalidArgument
evalT (HiExprDict l) = do
  let val = mapM (\(a, b) -> liftA2 (,) (evalT a) (evalT b)) l
  HiValueDict . M.fromList <$> val

eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval expr = runExceptT (evalT expr)
