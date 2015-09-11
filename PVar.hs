module PVar where

import Prelude
import Data.IORef
import Control.Exception
import Result
import qualified Err
import Text.Nicify (nicify)
import Utils (tryRead)
import Control.Applicative
  
data PVar a =
  FSPVar (Maybe (UpgradeFun a)) FilePath
  | IORefPVar (IORef a)
    
type UpgradeFun a = String -> Result a
    
newPVar :: FilePath -> PVar a
newPVar = FSPVar Nothing
            
newPVarUpg :: UpgradeFun a -> FilePath -> PVar a
newPVarUpg upg fp = FSPVar (Just upg) fp

fakePVar :: a -> IO (PVar a)
fakePVar x = IORefPVar <$> newIORef x
            
readPVar :: (Read a, Show a) => PVar a -> IO a
readPVar (FSPVar mUpg fp) = do
  content <- readFile fp
  case tryRead content of
   Right x -> return x
   Left directReadErr ->
     case mUpg of
       Nothing -> throwIO (Err.tag ("while reading " ++ fp) directReadErr)
       Just upg ->
        case upg content of
          Right x -> return x
          Left err2 ->
            throwIO (Err.tag ("can't read nor upgrade " ++ fp) (Err.List [directReadErr, err2]))
readPVar (IORefPVar ioref) = readIORef ioref

writePVar :: Show a => PVar a -> a -> IO ()
writePVar (FSPVar _ fp) x =
  writeFile fp (nicify . show $ x)
writePVar (IORefPVar ioref) x = writeIORef ioref x
