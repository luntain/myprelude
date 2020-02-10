{-# LANGUAGE PatternSynonyms #-}
-- based on https://github.com/janestreet/base/blob/master/src/or_error.mli
module ErrorOr (
  ErrorOr
  , Taggable(..)
  , pattern Error
  , pattern OK
  , isOK
  , isError
  , ok
  , err
)
where

import Data.Semigroup
import Data.List

data ErrorOr a = ErrorOr { toEither :: Either ErrorAcc a }
  deriving (Show, Read, Eq, Ord)

pattern OK :: a -> ErrorOr a
pattern OK x <- ErrorOr (Right x)

pattern Error :: ErrorAcc -> ErrorOr a
pattern Error err <- ErrorOr (Left err)

data ErrorAcc = EmptyError
              | Compose ErrorAcc ErrorAcc
              | Tag String ErrorAcc
              | Message String
              deriving (Show, Read, Eq, Ord)

instance Semigroup ErrorAcc where
  EmptyError <> x = x
  x <> EmptyError = x
  t1 <> t2        = Compose t1 t2

instance Monoid ErrorAcc where
  mempty = EmptyError
  mappend = (<>)

instance Functor ErrorOr where
  fmap f (ErrorOr (Right x)) = ErrorOr (Right (f x))
  fmap _ (ErrorOr (Left e))  = ErrorOr (Left e)

isOK :: ErrorOr a -> Bool
isOK (OK _) = True
isOK _ = False

-- Left of empty error is also an error
isError :: ErrorOr a -> Bool
isError = not . isOK

mapError :: (ErrorAcc -> ErrorAcc) -> ErrorOr a -> ErrorOr a
mapError f (ErrorOr (Left e)) = ErrorOr (Left (f e))
mapError _ ok = ok

ok :: a -> ErrorOr a
ok x = ErrorOr (Right x)

err :: String -> ErrorOr a
err str = ErrorOr (Left (Message str))

class Taggable c where
  tag :: String -> c -> c

instance Taggable ErrorAcc where
  tag = Tag

instance Taggable (ErrorOr a) where
  tag str res
    | isOK res = res
    | otherwise = mapError (Tag str) res
