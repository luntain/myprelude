module RecordParser2 where

import MyPrelude
import Data.String.Utils

newtype M a = M { unm :: [String] -> Result ([String] -> Result a) }

instance Functor M where
   fmap f (M g) = M (fmap (fmap . fmap . fmap $ f) g)

instance Applicative M where
  pure x = M (const (return (const (return x))))
  (M g) <*> (M f) = M $ \h -> case (g h, f h) of
                               (Left err, _) -> Left err
                               (_, Left err) -> Left err
                               (Right g', Right f') -> Right $ \r -> g' r <*> f' r

parse :: M a -> [[String]] -> Result [a]
parse (M f) (hdr:rows) =
  case f hdr of
    Left err -> Left err
    Right rParser -> catResults . map rParser $ rows

stringField :: String -> M String
stringField colTitle = M $ \hdr ->
  case elemIndex (map toLower colTitle) (map (map toLower . strip) hdr) of
    Nothing -> errorResult (printf "No such column '%s' in %s" colTitle (show hdr))
    Just i -> Right $ \r ->
      case drop i r of
        [] -> errorResult (printf "No %dth field for col '%s' in %s" i colTitle (show r))
        x:_ -> return x

fromField :: String -> (String -> Result a) -> M a
fromField n f =
    M $ \h ->
       case field h of
         Left err ->  Left err
         Right fn -> Right $ \r ->
           case fn r of
             Left err -> Left err
             Right res -> tag ("parsing field " ++ n) $ f res
  where M field = stringField n

fromFields :: [String] -> ([String] -> Result a) -> M a
fromFields ns f =
  M $ \hdr ->
    let ixs = catResults . map (\n -> toResultA n . elemIndex (map toLower n) . map (map toLower . strip) $ hdr) $ ns in
    fmap (g f) ixs
  where
    g :: ([String] -> Result a) -> [Int] -> [String] -> Result a
    g f ixs flds =
      let relevantFields = catResults . map (\ix -> toResultA (printf "no col %d for in %s" ix (show flds)) . findByIndex ix $ flds) $ ixs in
      relevantFields >>= f


findByIndex :: Int -> [a] -> Maybe a
findByIndex i xs =
  case drop i xs of
    [] -> Nothing
    x:_ -> return x
