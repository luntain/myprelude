module Utils (
  null,
  printf,
  lookup,
  lookupIn,
  tryRead,
  split,
  removeOrFail,
  readCommaDecimal,
  Failable(..), fail,
  zlotowki,
  mif, munless, mwhen, mfromMaybe
) where

import Prelude hiding (fail, null, lookup)
import qualified Err
import qualified Control.Exception as E
import qualified Data.Char
import Control.Monad hiding (fail)
import qualified Data.List as List
import Control.Monad.IO.Class
import System.IO
import Text.Printf
import qualified Data.Map as M

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

mwhen :: MonadIO m => m Bool -> m () -> m ()
mwhen cond exec = cond >>= flip when exec
  
munless :: Monad m => m Bool -> m () -> m ()
munless cond exec = cond >>= flip unless exec
                    
mif :: Monad m => m Bool -> m a -> m a -> m a
mif cond thn els = do
  cond <- cond
  if cond then thn else els
  
mfromMaybe :: Monad m => m a -> m (Maybe a) -> m a
mfromMaybe z m = do
  val <- m
  case val of
    Nothing -> z
    Just x -> return x
                  
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
zlotowki v = printf "%.2f zł" v -- use non-breaking space
