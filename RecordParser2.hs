module RecordParser2 where

import MyPrelude
import Data.String.Utils

data M a = M { unm :: [String] -> Result ([String] -> Result a) }

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
             Right res -> f res
  where M field = stringField n
