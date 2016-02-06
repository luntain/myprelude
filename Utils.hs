module Utils (
  null,
  printf,
  lookup,
  lookupIn,
  tryRead,
  split,
  removeOrFail,
  readCommaDecimal,
  tryReadCommaDecimal,
  Failable(..), failStr,
  zlotowki,
  mif, munless, mwhen, mfromMaybe,
  forceEither, forceRight, forceEitherMsg,
  polishTimeLocaleLc, diskCached,
  createIdGenerator
) where

import Prelude hiding (fail, null, lookup)
import System.IO
import Data.IORef (atomicModifyIORef', newIORef)
import qualified Err
import qualified Control.Exception as E
import qualified Data.Char
import Control.Monad hiding (fail)
import qualified Data.List as List
import Control.Monad.IO.Class
import Text.Printf
import Data.Time
import qualified Data.Map as M
import System.Directory


class    Monad m => Failable m   where failErr :: Err.T -> m a
instance Failable Maybe          where failErr _   = Nothing
instance Failable (Either Err.T) where failErr err = Left err
instance MonadIO m => Failable m where failErr err = liftIO (E.throwIO err)

failStr :: Failable m => String -> m a
failStr = failErr . Err.Msg

class OverloadedLookup t k v | t -> k, t -> v where overloadedLookup :: k -> t -> Maybe v
instance (Eq k) => OverloadedLookup [(k,v)] k v where overloadedLookup = List.lookup
instance (Ord k) => OverloadedLookup (M.Map k v) k v where overloadedLookup = M.lookup

lookup :: (OverloadedLookup t k v, Show k, Show t, Failable m) => k -> t -> m v
lookup k xs = lookupIn (shorten 256 (show xs)) k xs

-- this is a lookup variant for where there is no show instance for the collection
lookupIn :: (OverloadedLookup t k v, Show k, Failable m) => String -> k -> t -> m v
lookupIn name k = maybe (failStr $ printf "Can't lookup %s in %s" (show k) name) return . overloadedLookup k

-- todo this might actually be faster if one does a linear search to see if the element is absent ahead of time
removeOrFail :: (Show e, Failable m, Eq e) => e -> [e] -> m [e]
removeOrFail el [] = failStr ("Can't remove: no " ++ show el ++ " found")
removeOrFail el (e:es) = if e == el then return es else removeOrFail el es >>= return . (e:)

shorten :: Int -> String -> String
shorten maxLen msg =
  case List.splitAt maxLen msg of
    (x, "") -> x
    (x, _rest) -> take (maxLen - 5) x ++ "(...)"

tryRead :: (Read a, Show a, Failable m) => String -> m a
tryRead str =
  case reads str of
    [] -> failStr ("Can't read: " ++ shorten 256 str)
    [(a, rest)]
      | all Data.Char.isSpace rest -> return a
    parses -> failStr ("Ambiguous parse: '" ++ str ++ "' " ++ shorten 256 (show parses))

mwhen :: Monad m => m Bool -> m () -> m ()
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

tryReadCommaDecimal :: (Failable m, Read a, Show a, RealFloat a) => String -> m a
tryReadCommaDecimal = genReadCommaDecimal tryRead

readCommaDecimal :: (Read a, RealFloat a) => String -> a
readCommaDecimal = genReadCommaDecimal read

genReadCommaDecimal :: (String -> a) -> String -> a
genReadCommaDecimal f =
  f
  . map commaToDot
  . filter (/= ' ')
  . filter (/= ' ') -- epromak's non-breaking space in thousands
 where
   commaToDot ',' = '.'
   commaToDot x = x

zlotowki :: Double -> String
zlotowki v = printf "%.2f zł" v -- use non-breaking space

forceEither :: Show e => Either e a -> a
forceEither (Left e) = error ("Forced a Left: " ++ (show e))
forceEither (Right v) = v
forceRight :: Show e => Either e a -> a
forceRight = forceEither

forceEitherMsg :: Show e => String -> Either e a -> a
forceEitherMsg msg (Left e) = error ("Forced a Left: " ++ msg ++ " " ++ (show e))
forceEitherMsg _ (Right v) = v

createIdGenerator :: (Enum x, Num x) => IO (IO x)
createIdGenerator = do
  r <- newIORef 0
  return (atomicModifyIORef' r (\i -> (succ i, i)))

-- lower case
polishTimeLocaleLc =
  TimeLocale {wDays =
                 [("Niedziela","Nie")
                 ,("Poniedziałek","Pon")
                 ,("Wtorek","Wto")
                 ,("Środa","Śro")
                 ,("Czwartek","Czw"),("Piątek","Pią"),("Sobota","Sob")],
              months =
                [("styczenia","sty")
                ,("lutego","lut")
                ,("marca","mar")
                ,("kwietnia","kwi")
                ,("maja","maj")
                ,("czerwca","cze")
                ,("lipica","lip")
                ,("sierpnia","sie")
                ,("wrzesnia","wrz")
                ,("października","paź")
                ,("listopada","lis")
                ,("grudnia","gru")],
              amPm = ("AM","PM")
              , dateTimeFmt = "%a %b %e %H:%M:%S %Z %Y"
              , dateFmt = "%y-%m-%d"
              , timeFmt = "%H:%M:%S", time12Fmt = "%I:%M:%S %p"
              , knownTimeZones=[]}

diskCached :: (Eq a, Read a, Show a, Read r, Show r) => FilePath -> (a -> IO r) -> a -> IO r
diskCached file_path produce arg = do
  cv <- cached_value arg
  case cv of
   Just value -> return value
   Nothing -> do
     result <- produce arg
     writeFile file_path (show (arg, result))
     return result
  where
    cached_value arg = do
      exists <- doesFileExist file_path
      if not exists
        then return Nothing
        else do
          contents <- readFile file_path
          case reads contents of
           [((cachedKey, cachedResult), "")] ->
            if cachedKey == arg
              then return (Just cachedResult)
              else return Nothing
           _ -> error ("invalid cache file " ++ file_path)
