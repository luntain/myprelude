{-# LANGUAGE NoImplicitPrelude #-}
module MyPrelude (
  module Exports,
  Data.Traversable.mapM, Data.Traversable.forM,
  Data.Traversable.foldMapDefault,
  fromMaybe, fromJust, maybeToList, mapMaybe, filterMap, isNothing, isJust, listToMaybe, catMaybes,
  E.throwIO, E.throw,
  List.nub, List.sort, List.partition, List.sortBy, List.groupBy, List.inits, List.insertBy, List.isInfixOf,
  Data.Tuple.swap,
  Data.Ord.comparing,
  Data.Ord.Down(..),
  Data.Either.partitionEithers,
  CMR.ReaderT(..), CMR.ask, CMR.asks, CMR.runReaderT,
  CMW.WriterT(..), CMW.tell, CMW.runWriterT,
  Data.Functor.Identity.Identity(..),
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
import qualified Data.Either
import Data.Foldable as Exports hiding (null)
import Control.Arrow as Exports hiding (loop)
import Control.Applicative as Exports
import Control.Concurrent as Exports
import qualified Data.Traversable
import qualified Data.Map as M
import Data.Maybe (fromMaybe,fromJust,maybeToList,mapMaybe,isNothing,isJust,listToMaybe,catMaybes)
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
import PVar as Exports
import Utils as Exports
import qualified Control.Monad.Reader as CMR
import qualified Control.Monad.Writer as CMW
import qualified Data.Functor.Identity


filterMap = mapMaybe
