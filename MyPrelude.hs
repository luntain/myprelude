{-# LANGUAGE NoImplicitPrelude #-}
module MyPrelude (
  module Exports,
  Data.Traversable.mapM, Data.Traversable.forM,
  Data.Traversable.foldMapDefault,
  fromMaybe, fromJust, maybeToList, mapMaybe, filterMap, isNothing, isJust, listToMaybe,
  E.throwIO, E.throw,
  printf,
  lookup,
  lookupIn,
  tryRead,
  Failable(..), fail,
  whenM, unlessM, ifM,
  List.nub, List.sort, List.partition, List.sortBy, List.groupBy,
  null,
  Data.Tuple.swap,
  split,
  removeOrFail,
  readCommaDecimal,
  Data.Ord.comparing,
  Data.Ord.Down(..),
  zlotowki
  ) where

import Prelude as Exports hiding (mapM, mapM_, lookup, fail, null, all, any, product, sum, and, or,
                                  elem, notElem, concatMap, foldl, foldl1, foldr, foldr1, maximum, minimum,
                                  sequence_, concat)
import System.IO as Exports
import Data.IORef as Exports
import Control.Monad as Exports hiding (mapM, mapM_, fail, sequence_, forM_, forM, msum)
import Control.Monad.IO.Class as Exports
import System.Timeout as Exports
import Data.Monoid as Exports
import Data.Foldable as Exports
import Control.Arrow as Exports
import Control.Applicative as Exports
import Control.Concurrent as Exports
import qualified Data.Traversable
import qualified Data.Map as M
import Data.Maybe (fromMaybe,fromJust,maybeToList,mapMaybe,isNothing,isJust,listToMaybe)
import Text.Printf
import Result as Exports
import qualified Err
import System.FilePath as Exports ((</>))
import qualified Data.List as List
import qualified Control.Exception as E
import qualified Data.Char 
import qualified Data.Tuple
import Data.Function as Exports
import qualified Data.Ord

class    Monad m => Failable m   where failErr :: Err.T -> m a
instance Failable Maybe          where failErr _   = Nothing
instance Failable (Either Err.T) where failErr err = Left err
instance MonadIO m => Failable m where failErr err = liftIO (E.throwIO err)
                                       
fail :: Failable m => String -> m a
fail = failErr . Err.Msg

class OverloadedLookup t k v | t -> k, t -> v where overloadedLookup :: k -> t -> Maybe v
instance (Eq k) => OverloadedLookup [(k,v)] k v where overloadedLookup = List.lookup
instance (Ord k) => OverloadedLookup (M.Map k v) k v where overloadedLookup = M.lookup

lookup :: (OverloadedLookup t k v, Show k, Show t, Failable m) => k -> t -> m v
lookup k xs = lookupIn (shorten 256 (show xs)) k xs

-- this is a lookup variant for where there is no show instance for the collection
lookupIn :: (OverloadedLookup t k v, Show k, Failable m) => String -> k -> t -> m v
lookupIn name k = maybe (fail $ printf "Can't lookup %s in %s" (show k) name) return . overloadedLookup k
                  
-- todo this might actually be faster if one does a linear search to see if the element is absent ahead of time
removeOrFail :: (Show e, Failable m, Eq e) => e -> [e] -> m [e]
removeOrFail el [] = fail ("Can't remove: no " ++ show el ++ " found")
removeOrFail el (e:es) = if e == el then return es else removeOrFail el es >>= return . (e:)

shorten :: Int -> String -> String
shorten maxLen msg =
  case List.splitAt maxLen msg of
    (x, "") -> x
    (x, _rest) -> take (maxLen - 5) x ++ "(...)"

tryRead :: (Read a, Show a, Failable m) => String -> m a
tryRead str =
  case reads str of
    [] -> fail ("Can't read: " ++ shorten 256 str)
    [(a, rest)]
      | all Data.Char.isSpace rest -> return a
    parses -> fail ("Ambiguous parse: '" ++ str ++ "' " ++ shorten 256 (show parses))

whenM :: MonadIO m => m Bool -> m () -> m ()
whenM cond exec = cond >>= flip when exec
  
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM cond exec = cond >>= flip unless exec
                    
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM cond thn els = do
  cond <- cond
  if cond then thn else els
                  
class Nullable c where
  null :: c a -> Bool
  
instance Nullable [] where
  null [] = True
  null _ = False
  
instance Nullable Maybe where
  null Nothing = True
  null _ = False

split :: (Char -> Bool) -> String -> [String]
split _ [] = []
split p str =
  case break p str of
   (x,"") -> [x]
   (x, _:rest) -> x:split p rest

filterMap = mapMaybe

readCommaDecimal :: (Read a, RealFloat a) => String -> a
readCommaDecimal =
  read
  . map commaToDot
  . filter (/= ' ')
  . filter (/= ' ') -- epromak's non-breaking space in thousands
 where
   commaToDot ',' = '.'
   commaToDot x = x

zlotowki :: Double -> String
zlotowki v = printf "%.2f zł" v
