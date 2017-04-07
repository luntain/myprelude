{-# LANGUAGE NoImplicitPrelude #-}
import MyPrelude
import Criterion.Main
import qualified Data.DList as DL
import qualified Data.List.Ordered as OL
import qualified Data.Map as M


testData = [(i, 1) | i <- [1..10000]]
input :: [[(Int,Int)]]
input = take 5 . repeat $ testData


-- this is me trying to check what is the optimal way of folding values from multiple association lists
-- by their keys assuming the association lists are sorted (actualy the map variant does not depend on it,
-- but it is slower than any other method + sorting anyway)
main = defaultMain [
    bgroup "folding association lists"
      [bench "using unionAL with DLists" $ nf (foldl1' (unionAL (<>)) . map (map (second DL.singleton))) input
      ,bench "using foldlAL" $ nf (foldlAL (flip (:)) singleton . map (sortWith fst)) input
      ,bench "using foldrAL" $ nf (foldrAL (:) singleton) input
      ,bench "usnig OL.mergeBy with group" $
            nf (map (fst.head &&& map snd) . groupBy ((==) `on` fst) . OL.mergeAllBy (compare `on` fst)) input
      ,bench "using maps" $ nf (M.unionsWith (<>) . map (M.fromList . map (second DL.singleton))) input
      ]]
