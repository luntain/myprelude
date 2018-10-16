{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
module Utils (
  printf,
  lookup,
  lookupIn,
  tryLast,
  tryHead,
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
  createIdGenerator,
  sec,
  containsCC,
  insideCC,
  unionAL,
  foldlAL,
  foldrAL,
  zipWithAL,
  singleton,
  writeFileAtomically,
  bool, guarded,
  insertPrependHM
, Opaque (..)
, Taggable (..)
, pluckFirst
, breakIntoGroups
, memoizeWithAMap
) where

import Prelude hiding (fail, null, lookup)
import System.IO
import Data.IORef (atomicModifyIORef', newIORef, modifyIORef, readIORef)
import qualified Err
import qualified Control.Exception as E
import qualified Data.Char
import Control.Monad hiding (fail)
import qualified Data.List as List
import Control.Monad.IO.Class
import Text.Printf
import Data.Time
import qualified Data.Map.Strict as M
import System.Directory
import Control.Arrow
import Control.Applicative (Alternative, empty)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM
import qualified Err
import Result


class    Monad m => Failable m   where failErr :: Err.T -> m a
instance {-# OVERLAPPING #-} Failable Maybe          where failErr _   = Nothing
instance {-# OVERLAPPING #-} Failable (Either Err.T) where failErr err = Left err
-- this is overlappable because type constraints are ignored
instance {-# OVERLAPPABLE #-} (Monad m, MonadIO m) => Failable m where failErr err = liftIO (E.throwIO err)

failStr :: Failable m => String -> m a
failStr = failErr . Err.Msg

class    Taggable a                 where tag :: String -> a -> a
instance Taggable Err.T             where tag = Err.tag
instance Taggable (Result.Result a) where tag = Result.tagResult

sec :: Int -- sec in microseconds
sec = 1000 * 1000

class OverloadedLookup t k v | t -> k, t -> v where overloadedLookup :: k -> t -> Maybe v
instance (Eq k) => OverloadedLookup [(k,v)] k v where overloadedLookup = List.lookup
instance (Ord k) => OverloadedLookup (M.Map k v) k v where overloadedLookup = M.lookup
instance (Eq k, Hashable k) => OverloadedLookup (HM.HashMap k v) k v where overloadedLookup = HM.lookup

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

tryLast :: (Failable m) => [a] -> m a
tryLast [] = failStr "called tryLast on empty list"
tryLast xs = return (last xs)

tryHead :: (Failable m) => [a] -> m a
tryHead (x:_) = return x
tryHead [] = failStr "tryHead on an empty list"

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

-- I wonder what is the performance of that funtion
-- Perhaps it is the reason why it is not in the standard lib
pluckFirst :: (a -> Bool) -> [a] -> (Maybe a, [a])
pluckFirst _p [] = (Nothing, [])
pluckFirst p  (a:as)
  | p a = (Just a, as)
  | otherwise = second (a:) (pluckFirst p as)

-- lower case
polishTimeLocaleLc =
  TimeLocale {wDays =
                 [("Niedziela","Nie")
                 ,("Poniedziałek","Pon")
                 ,("Wtorek","Wto")
                 ,("Środa","Śro")
                 ,("Czwartek","Czw"),("Piątek","Pią"),("Sobota","Sob")],
              months =
                [("stycznia","sty")
                ,("lutego","lut")
                ,("marca","mar")
                ,("kwietnia","kwi")
                ,("maja","maj")
                ,("czerwca","cze")
                ,("lipca","lip")
                ,("sierpnia","sie")
                ,("września","wrz")
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

containsCC :: Ord a => (a, a) -> (a, a) -> Bool
containsCC (a1, a2) (b1, b2) = a1 <= b1 && b2 <= a2

insideCC :: Ord a => (a, a) -> a -> Bool
insideCC (from, to) a = from <= a && a <= to


instance Read NominalDiffTime where
  readsPrec p s =
    let seconds = readsPrec p s :: [(Double, String)] in
    fmap (second tail . first (fromRational.toRational)) seconds

unionAL :: Ord k => (a -> a -> a) -> [(k, a)] -> [(k, a)] -> [(k, a)]
unionAL f as bs = zipWithAL f id as bs


foldlAL :: Ord k => (b -> a -> b) -> (a -> b) -> [[(k, a)]] -> [(k, b)]
foldlAL f z xs = go [] xs
  where go acc [] = acc
        go acc (xs:xss) = go (zipWithAL f z acc xs) xss

foldrAL :: Ord k => (a -> b -> b) -> (a -> b) -> [[(k, a)]] -> [(k, b)]
foldrAL f z [] = []
foldrAL f z (xs:xss) = zipWithAL (flip f) z (foldrAL f z xss) xs

zipWithAL :: Ord k => (z -> a -> z) -> (a -> z) -> [(k, z)] -> [(k, a)] -> [(k, z)]
zipWithAL _f _z zs [] = zs
zipWithAL f z [] xs = map (second z) xs
zipWithAL f z (a:as) (x:xs)
  | fst a < fst x = a : zipWithAL f z as (x:xs)
  | fst a > fst x = (fst x, z (snd x)) : zipWithAL f z (a:as) xs
  | otherwise = fmap (f (snd a)) x : zipWithAL f z as xs

-- I was surprised that singleton was not defined in Data.List
singleton :: a -> [a]
singleton x = [x]

-- this function is invalid to windows
writeFileAtomically :: FilePath -> String -> IO ()
writeFileAtomically fp content = do
  tempdir <- getTemporaryDirectory
  (tfp, h) <- openBinaryTempFileWithDefaultPermissions tempdir (last $ split (=='/') fp)
  hPutStr h content
  hClose h
  renameFile tfp fp

bool :: a -> a -> Bool -> a
bool f t p = if p then t else f

guarded :: Alternative f => (a -> Bool) -> a -> f a
guarded p x = bool Control.Applicative.empty (pure x) (p x)

-- break between any two elements that satisfy given predicate
breakIntoGroups :: (a -> a -> Bool) -> [a] -> [[a]]
breakIntoGroups _ [] = []
breakIntoGroups break (x:rest) = let (group, rest') = f x rest in group : breakIntoGroups break rest'
  where
    f prev [] = ([prev], [])
    f prev (x:rest)
      | prev `break` x = ([prev],rest)
      | otherwise = first (prev:) (f x rest)


-- this is often used abstraction, perhaps too often
insertPrependHM :: (Ord k, Hashable k) => k -> v -> HM.HashMap k [v] -> HM.HashMap k [v]
insertPrependHM k v = HM.insertWith (\_new old -> v : old) k [v]

newtype Opaque a = Opaque { unOpaque :: a }
instance Show (Opaque a) where show = const "<opaque>"
instance Eq (Opaque a) where (==) = const (const False)


-- single threaded please
memoizeWithAMap :: Ord a => (a -> IO b) -> IO (a -> IO (Result b))
memoizeWithAMap f = do
  cache <- newIORef M.empty
  return $ \key -> do
    cache' <- readIORef cache
    case M.lookup key cache' of
      Just res -> return res
      Nothing -> do
        res <- tryResult $ f key
        modifyIORef cache (M.insert key res)
        return res
