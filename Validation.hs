module Validation where

import Prelude
import Utils
import qualified Err
import Data.Monoid
  
  
(>!) :: (Ord a, Show a) => a -> a -> Err.T
a >! b = if a > b then mempty else Err.Msg (show a ++ " is not greater than " ++ show b)

(>=!) :: (Ord a, Show a) => a -> a -> Err.T
a >=! b = if a >= b then mempty else Err.Msg (show a ++ " is not >= than " ++ show b)

(<!) :: (Ord a, Show a) => a -> a -> Err.T
a <! b = if a < b then mempty else Err.Msg (show a ++ " is not smaller than " ++ show b)

(<=!) :: (Ord a, Show a) => a -> a -> Err.T
a <=! b = if a <= b then mempty else Err.Msg (show a ++ " is not <= than " ++ show b)
                                   
(=!) :: (Eq a, Show a) => a -> a -> Err.T
a =! b = if a == b then mempty else Err.Msg (show a ++ " is not equal to " ++ show b)
                                   
isNothing :: Show a => Maybe a -> Err.T
isNothing Nothing = mempty
isNothing x = Err.Msg ("Expected Nothing, but got " ++ show x)

isJust :: Maybe a -> Err.T
isJust Nothing = Err.Msg ("Expected Just, but got Nothing")
isJust (Just _) = mempty
              

validate :: Failable m => Err.T -> m ()
validate e =
  if e == mempty then return () else failErr e
