{-# LANGUAGE NoImplicitPrelude #-}
import MyPrelude
import Test.Tasty
import Test.Tasty.QuickCheck
import qualified Data.List.Ordered as OL
import Data.Int


main = defaultMain tests

tests =
    testGroup "Assoc Lists" [
        testProperty "equivalance of foldrAL foldlAL, and mergeBy" (
         \randomLists ->
           let controlFold = map (fst.head &&& map snd) . groupBy ((==) `on` fst) . OL.mergeAllBy (compare `on` fst) . sortWith (listToMaybe) in
           let assocLists = map (OL.nubSortOn' fst) randomLists :: [[(Int8, Int8)]] in
           let l = foldlAL (flip (:)) singleton assocLists in
           let r = foldrAL (:)        singleton assocLists in
           let c = controlFold assocLists in
           let s = sortWith fst . map (second sort) in
           s l === s r .&&. s r === s c
           )
    ]
