{-# LANGUAGE NoImplicitPrelude #-}
module MyPrelude (
  module Exports,
  Data.Traversable.mapM, Data.Traversable.forM,
  Data.Traversable.foldMapDefault,
  fromMaybe, fromJust, maybeToList, Data.Maybe.mapMaybe, filterMap, isNothing, isJust, listToMaybe, catMaybes,
  E.throwIO, E.throw,
  List.nub, List.nubBy, List.sort, List.partition, List.sortBy, List.groupBy, List.inits, List.insertBy, List.isInfixOf, List.isSuffixOf, List.unfoldr, List.isPrefixOf, List.intercalate, List.findIndex, List.elemIndex, List.mapAccumL, (List.\\), List.foldl1',
  GHC.Exts.sortWith,
  Data.Tuple.swap,
  Data.Ord.comparing,
  Data.Ord.Down(..),
  Data.Either.partitionEithers,
  CMR.ReaderT(..), CMR.ask, CMR.asks,
  CMW.WriterT(..), CMW.tell,
  CMS.StateT(..), CMS.execStateT, CMS.modify,
  Data.Functor.Identity.Identity(..),
  Data.Char.toLower,
  -- IsList, IsString
  GHC.Exts.IsList(fromList),
  Data.String.IsString(..),
  -- often used types:
  Text,
  HM.HashMap,
  HS.HashSet,
  LByteString,
  BS.ByteString
  ) where

import Prelude as Exports hiding (mapM, mapM_, lookup, null, all, any, product, sum, and, or,
                                  elem, notElem, concatMap, foldl1, foldr, foldr1, maximum, minimum,
                                  sequence_, concat)
import System.IO as Exports
import Data.IORef as Exports
import Control.Monad as Exports hiding (mapM, mapM_, fail, sequence_, forM_, forM, msum)
import Control.Monad.IO.Class as Exports
import System.Timeout as Exports
import Data.Monoid as Exports
import qualified Data.Either
import Data.Foldable as Exports
import Control.Arrow as Exports hiding (loop)
import Control.Applicative as Exports
import Control.Concurrent as Exports
import qualified Data.Traversable
import Data.Maybe (fromMaybe,fromJust,maybeToList,mapMaybe,isNothing,isJust,listToMaybe,catMaybes)
import Text.Printf
import Result as Exports
import System.FilePath as Exports ((</>))
import qualified Data.List as List
import qualified Control.Exception as E
import qualified Data.Tuple
import Data.Function as Exports
import qualified Data.Ord
import PVar as Exports
import Utils as Exports
import qualified Control.Monad.Reader as CMR
import qualified Control.Monad.Writer as CMW
import qualified Control.Monad.State as CMS
import qualified Data.Functor.Identity
import qualified Data.Char
import qualified GHC.Exts
import Data.Default as Exports
import Data.Text (Text)
import Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.String

type LByteString = LBS.ByteString

filterMap = Data.Maybe.mapMaybe
