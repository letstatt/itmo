{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs  #-}

module HW3.Action
  (
    HiPermission(..),
    PermissionException(..),
    HIO(..),
  )
where
import Control.Exception (Exception, throwIO)
import qualified Data.ByteString as B
import qualified Data.Sequence as S
import Data.Set (Set, member)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import Data.Time (getCurrentTime)
import HW3.Base (HiAction (..), HiMonad (..), HiValue (..))
import System.Directory (createDirectory, doesDirectoryExist, doesFileExist, getCurrentDirectory,
                         listDirectory, setCurrentDirectory)
import System.Random (getStdRandom, uniformR)

data HiPermission
  = AllowRead
  | AllowWrite
  | AllowTime
  deriving (Show, Ord, Eq)

newtype PermissionException
  = PermissionRequired HiPermission
  deriving (Show)

instance Exception PermissionException

newtype HIO a = HIO
  { runHIO :: Set HiPermission -> IO a }
  deriving Functor

instance Applicative HIO where
  pure :: a -> HIO a
  pure v = HIO (\_ -> do pure v)
  (<*>) :: HIO (a -> b) -> HIO a -> HIO b
  (HIO f) <*> (HIO x) = HIO (\permissions -> do
    f permissions <*> x permissions)

instance Monad HIO where
  (>>=) :: HIO a -> (a -> HIO b) -> HIO b
  (HIO x) >>= f = HIO (\permissions -> do
    y <- x permissions
    let (HIO z) = f y in
      z permissions)

getHIOWithPermissionsCheck :: HiPermission -> IO HiValue -> HIO HiValue
getHIOWithPermissionsCheck permission io = HIO (\permissions ->
  if member permission permissions
    then io
    else throwIO $ PermissionRequired permission)

instance HiMonad HIO where
  runAction :: HiAction -> HIO HiValue
  runAction (HiActionRead p) = getHIOWithPermissionsCheck AllowRead (
    do
      isFile <- doesFileExist p
      isDirectory <- doesDirectoryExist p
      if isFile then do
        b <- B.readFile p
        pure $ either (const $ HiValueBytes b) HiValueString . decodeUtf8' $ b
      else if isDirectory then
        (HiValueList . S.fromList) . map (HiValueString . T.pack) <$> listDirectory p
      else pure HiValueNull)

  runAction (HiActionWrite p b) = getHIOWithPermissionsCheck AllowWrite (
    HiValueNull <$ B.writeFile p b)

  runAction (HiActionMkDir p) = getHIOWithPermissionsCheck AllowWrite (
    HiValueNull <$ createDirectory p)

  runAction (HiActionChDir p) = getHIOWithPermissionsCheck AllowRead (
    HiValueNull <$ setCurrentDirectory p)

  runAction HiActionCwd = getHIOWithPermissionsCheck AllowRead (
    HiValueString . T.pack <$> getCurrentDirectory)

  runAction HiActionNow = getHIOWithPermissionsCheck AllowTime (
    HiValueTime <$> getCurrentTime)

  runAction (HiActionRand l r) = HIO (\_ ->
    HiValueNumber . toRational <$> getStdRandom (uniformR (l, r)))

  runAction (HiActionEcho s) = getHIOWithPermissionsCheck AllowWrite (
    HiValueNull <$ putStrLn (T.unpack s))
