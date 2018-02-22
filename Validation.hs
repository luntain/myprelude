module Validation where

import Utils
import qualified Err
import Control.Lens


-- <> is infixr 6 :|

infix 4 >!
(>!) :: (Ord a, Show a) => a -> a -> Err.T
a >! b = if a > b then mempty else Err.Msg (show a ++ " is not greater than " ++ show b)

infix 4 >=!
(>=!) :: (Ord a, Show a) => a -> a -> Err.T
a >=! b = if a >= b then mempty else Err.Msg (show a ++ " is not >= than " ++ show b)

infix 4 <!
(<!) :: (Ord a, Show a) => a -> a -> Err.T
a <! b = if a < b then mempty else Err.Msg (show a ++ " is not smaller than " ++ show b)

infix 4 <=!
(<=!) :: (Ord a, Show a) => a -> a -> Err.T
a <=! b = if a <= b then mempty else Err.Msg (show a ++ " is not <= than " ++ show b)

infix 4 =!
(=!) :: (Eq a, Show a) => a -> a -> Err.T
a =! b = if a == b then mempty else Err.Msg (show a ++ " is not equal to " ++ show b)

infix 4 /=!
(/=!) :: (Eq a, Show a) => a -> a -> Err.T
a /=! b = if a/=b then mempty else Err.Msg (show a ++ " is equal to " ++ show b)

isNothing :: Show a => Maybe a -> Err.T
isNothing Nothing = mempty
isNothing x = Err.Msg ("Expected Nothing, but got " ++ show x)

isJust :: Maybe a -> Err.T
isJust Nothing = Err.Msg ("Expected Just, but got Nothing")
isJust (Just _) = mempty

all :: Show a => (a -> Err.T) -> [a] -> Err.T
all p = mconcat . map p'
  where p' x = if p x /= mempty
                  then Err.tag (show x) (p x)
                  else mempty

ensure :: String -> Bool -> Err.T
ensure label condition = if condition then mempty else Err.Msg label

validate :: Failable m => Err.T -> m ()
validate e =
  if e == mempty then return () else failErr e
