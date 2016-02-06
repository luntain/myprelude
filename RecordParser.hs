module RecordParser where

import Prelude
import Result
import Text.Printf

data M a = M { unm :: [String] -> Result a }

instance Functor M where
  fmap f (M g) = M (fmap f . g)

instance Applicative M where
  pure x = return x
  (M g) <*> (M f) = M (\r -> g r <*> f r)

instance Monad M where
  return x = M (const (return x))
  M f >>= g = M (\inp -> case f inp of
                               Left err -> Left err
                               Right res -> unm (g res) inp)
  fail msg = M (const $ errorResult msg)

getField :: Int -> [String] -> Result String
getField i l
  | i > -1 = case drop i l of
                [] -> errorResult (printf "No %dth field in %s" i (show l))
                x:_ -> return x
  | otherwise = errorResult (printf "Invalid field index %d" i)


parse :: M a -> [String] -> Result a
parse (M f) = f

stringField :: Int -> M String
stringField n = M (getField n)

fromField :: Int -> (String -> Result a) -> M a
fromField n f =
  M (\r -> getField n r >>= f)
