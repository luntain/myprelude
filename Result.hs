{-# LANGUAGE NoImplicitPrelude, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}
module Result where

import Prelude hiding (lookup)
import Control.Applicative
import Data.Monoid
import Data.Either
import System.Exit
import qualified Control.Exception as Exc
import qualified Err


errorResult :: String -> Result a
errorResult msg = Left (Err.Msg msg)

concatResults :: [Result a] -> Result [a]
concatResults results =
  case partitionEithers results of
    ([], rights) -> Right rights
    (errs, _) -> Left (mconcat errs)

catResults = concatResults

tagResult title (Left err) = Left (Err.Tag title err)
tagResult _ r = r

type Result a = Either Err.T a

instance Monoid a => Monoid (Result a) where
  mempty = Right mempty
  mappend (Right a)  (Right b) = Right (mappend a b)
  mappend (Left err) (Right _) = Left err
  mappend (Right _) (Left err) = Left err
  mappend (Left err1) (Left err2) = Left (mappend err1 err2)


-- I forgot about it, and it is somewhat odd, to I ever use it?
-- this is different from the regular Either instance that
-- it does not throw an exception for `fail`
instance {-# OVERLAPPING #-} Monad (Either Err.T) where
  Left  l >>= _ = Left l
  Right r >>= k = k r
  return = Right
  fail = errorResult

instance {-# OVERLAPPING #-} Alternative (Either Err.T) where
  empty = errorResult "None"
  Left err1 <|> Left err2 = Left (err1 <> err2)
  Right x <|> _ = Right x
  Left _ <|> Right x = Right x

prettyResult :: (Show a) => Result a -> String
prettyResult (Right x) = "(Right (" ++ show x ++ "))"
prettyResult (Left err) = "(Left (" ++ pe err ++ "))"
       -- if the err is multiline, force it to start on a new line
 where pe e@(Err.List (_:_)) = "\n" ++ Err.pretty 4 e
       pe e@(Err.Tag _ _) = "\n" ++ Err.pretty 4 e
       pe e = Err.pretty 0 e


       -- fromExn
resultFromExn :: Exc.Exception e => Either e a -> Result a
resultFromExn (Left e) = Left . Err.Msg . show . Exc.toException $ e
resultFromExn (Right x) = Right x

-- this captures both Err and IOError
tryResult :: IO a -> IO (Result a)
tryResult action = do
  x <- Exc.try (Exc.try (Exc.try action))
  case x of
   Left e -> return (Left (Err.Msg . show . Exc.toException $ (e::Exc.IOException))) -- why Exc.toException?
   Right (Left e) -> return (Left (Err.Msg . show $ (e::Exc.ErrorCall)))
   Right (Right (Left e)) -> return (Left e)
   Right (Right (Right x)) -> return (Right x)

-- It is defined in Result module rather than Err module because it depends on tryResult
-- like try from this module, it catches both Err and IOError
catchResult :: IO a -> (Err.T -> IO a) -> IO a
catchResult action handle =
  tryResult action >>= either handle return

handleResult :: (Err.T -> IO a) -> IO a -> IO a
handleResult = flip catchResult

class ConvertToResult a b where
  toResult :: a -> b
  -- from' adds an err annotation
  toResultA :: String -> a -> b
  toResultA _ = toResult

instance Show e => ConvertToResult (Either e a) (Result a) where
  toResult (Left x) = Left (Err.Msg (show x))
  toResult (Right x) = Right x

instance ConvertToResult (Maybe a) (Result a) where
  toResult Nothing = errorResult "Nothing"
  toResult (Just a) = Right a
  toResultA adnotation Nothing = errorResult adnotation
  toResultA _ (Just a) = Right a

instance ConvertToResult (Maybe a) (IO a) where
  toResult Nothing = Err.throwIO "Nothing"
  toResult (Just a) = return a -- this hardly makes sense
  toResultA adnotation Nothing = Err.throwIO adnotation
  toResultA _ (Just a) = return a

instance ConvertToResult ExitCode (Result ()) where
  toResult ExitSuccess = Right ()
  toResult (ExitFailure x) = Left (Err.Msg ("Exit failure code was: " ++ show x))

instance ConvertToResult (Result a) (IO a) where
  toResult (Left e) = Exc.throwIO e
  toResult (Right x) = return x
  toResultA msg (Left e) = Exc.throwIO (Err.Tag msg e)
  toResultA _ (Right x) = return x

forceResult :: String -> Result a -> IO a
forceResult = toResultA

forceResultUnsafe :: Result a -> a
forceResultUnsafe (Left e) = error ("Forced an Error: " ++ (show e))
forceResultUnsafe (Right v) = v

resultToMaybe :: Result a -> Maybe a
resultToMaybe (Left _) = Nothing
resultToMaybe (Right a) = Just a
