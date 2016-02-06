module Err (T(..), pretty, throwIO, tag) where

import Prelude
import Data.List
import Data.Typeable
import qualified Control.Exception as Exc

data T =
    Msg String
  | List [T]
  | Tag String T
  deriving (Read, Show, Typeable, Eq)

instance Exc.Exception T

instance Monoid T where
  mempty = List []
  mappend (List xs) (List ys) = List (xs ++ ys)
  mappend (List xs) x = List (xs ++ [x])
  mappend x (List xs) = List (x:xs)
  mappend x y = List [x,y]

pretty :: Int -> T -> String
pretty indent (Msg str) = replicate indent ' ' ++ str
pretty indent (List errs) = intercalate "\n" . map (pretty indent) $ errs
pretty indent (Tag str err) = intercalate "\n" [pretty indent (Msg str), pretty (indent + 4) err]

throwIO :: String -> IO a
throwIO = Exc.throwIO . Msg

tag :: String -> T -> T
tag _ x@(List []) = x
tag n x = Tag n x
