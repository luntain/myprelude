module Validation where

import Utils
import qualified Err
import Result


-- <> is infixr 6 :|

infix 4 >!
(>!) :: (Ord a, Show a) => a -> a -> Result ()
a >! b = if a > b then Right () else errorResult (show a ++ " is not greater than " ++ show b)

infix 4 >=!
(>=!) :: (Ord a, Show a) => a -> a -> Result ()
a >=! b = if a >= b then mempty else errorResult (show a ++ " is not >= than " ++ show b)

infix 4 <!
(<!) :: (Ord a, Show a) => a -> a -> Result ()
a <! b = if a < b then mempty else errorResult (show a ++ " is not smaller than " ++ show b)

infix 4 <=!
(<=!) :: (Ord a, Show a) => a -> a -> Result ()
a <=! b = if a <= b then mempty else errorResult (show a ++ " is not <= than " ++ show b)

infix 4 =!
(=!) :: (Eq a, Show a) => a -> a -> Result ()
a =! b = if a == b then mempty else errorResult (show a ++ " is not equal to " ++ show b)

infix 4 /=!
(/=!) :: (Eq a, Show a) => a -> a -> Result ()
a /=! b = if a/=b then mempty else errorResult (show a ++ " is equal to " ++ show b)

ensureIsNothing :: Show a => Maybe a -> Result ()
ensureIsNothing Nothing = mempty
ensureIsNothing x = errorResult ("Expected Nothing, but got " ++ show x)

ensureIsJust :: Maybe a -> Result ()
ensureIsJust Nothing = errorResult ("Expected Just, but got Nothing")
ensureIsJust (Just _) = mempty

ensureAll :: Show a => (a -> Result ()) -> [a] -> Result ()
ensureAll p = mconcat . map p'
  where p' x = if p x /= mempty
                  then tag (show x) (p x)
                  else mempty

ensure :: String -> Bool -> Result ()
ensure label condition = if condition then mempty else errorResult label

approxEqual :: (RealFrac a, Show a) => Double -> a -> a -> Result ()
approxEqual ratio x y =
  if abs (realToFrac x - realToFrac y) <= abs (ratio * avg)
    then mempty
    else errorResult ("The numbers are too far apart: " ++ show x ++ " " ++ show y)
  where
    avg :: Double
    avg = realToFrac (x + y) / 2

infix 3 ==>
(==>) :: Bool -> Result () -> Result ()
cond ==> r = if cond then r else pure ()
