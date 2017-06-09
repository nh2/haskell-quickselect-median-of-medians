-- TODO: Implement it for vectors, pull-request it into vector-algorithms

-- Based on http://austinrochford.com/posts/2013-10-29-median-of-medians-in-haskell.html
-- (which is slightly wrong because it can't deal with repeated elements,
-- see https://github.com/AustinRochford/blog-content/commit/ab32c7ab1ca1cd1555b87ec4edbeecbf03cad170#commitcomment-22433560

{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Control.DeepSeq (deepseq)
import           Data.List (sort, partition)
import           Data.Vector (Vector, (!))
import qualified Data.Vector as V
import           GHC.Exts (build)
import qualified Data.Vector.Algorithms.Heap as Heap
import qualified Data.Vector.Algorithms.Merge as Merge
import qualified Data.Vector.Algorithms.Intro as Intro
import           System.Random

import           Criterion.Main
import           Test.QuickCheck


median :: (Ord a) => [a] -> a
median = medianOfMedians


medianOfMedians :: (Ord a) => [a] -> a
medianOfMedians xs = select (((length xs) - 1) `quot` 2) xs


select :: (Ord a) => Int -> [a] -> a
select i xs
  | null (drop 5 xs) = sort xs !! i -- checks if the list length is <= 5 without computing the full length
  | k <= i && i < k+e = pivot
  | i < k = select i smaller
  | otherwise = select (i - k - 1) (drop 1 equal ++ larger)
  where
    pivot = medianOfMedians (map medianSpecializedTo5 (chunksOf 5 xs))
    (smaller, equal, larger) = pivotPartition xs pivot
    k = length smaller
    e = length equal


pivotPartition :: (Ord a) => [a] -> a -> ([a], [a], [a])
pivotPartition xs pivot = (smaller, equal, larger)
  where
    smaller = filter (< pivot) xs
    equal = filter (== pivot) xs
    larger = filter (> pivot) xs


chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (build (splitter ls)) where
  splitter :: [e] -> ([e] -> a -> a) -> a -> a
  splitter [] _ n = n
  splitter l c n  = l `c` splitter (drop i l) c n


medianSpecializedTo5 :: (Ord a) => [a] -> a
medianSpecializedTo5 [a,b,c,d,e] = thirdSmallestOf5 a b c d e
medianSpecializedTo5 xs = medianBySort xs


selectBySort :: (Ord a) => Int -> [a] -> a
selectBySort i xs
  | i > length xs = error $ "selectBySort: index " ++ show i ++ " < list length " ++ show (length xs)
  | otherwise = sort xs !! i

medianBySort :: (Ord a) => [a] -> a
medianBySort xs = selectBySort ((length xs - 1) `quot` 2) xs


thirdSmallestOf5 :: (Ord a) => a -> a -> a -> a -> a -> a
thirdSmallestOf5 a b c d e
  | a_smaller_count == 0 = a
  | b_smaller_count == 0 = b
  | c_smaller_count == 0 = c
  | d_smaller_count == 0 = d
  | e_smaller_count == 0 = e
  | otherwise = error "thirdSmallestOf5: cannot happen"
  where
    ab = if a < b then 1 else -1
    ac = if a < c then 1 else -1
    ad = if a < d then 1 else -1
    ae = if a < e then 1 else -1

    bc = if b < c then 1 else -1
    bd = if b < d then 1 else -1
    be = if b < e then 1 else -1

    cd = if c < d then 1 else -1
    ce = if c < e then 1 else -1

    de = if d < e then 1 else -1

    a_smaller_count =  ab + ac + ad + ae
    b_smaller_count = -ab + bc + bd + be
    c_smaller_count = -ac - bc + cd + ce
    d_smaller_count = -ad - bd - cd + de
    e_smaller_count = -ae - be - ce - de


sortedVectorMedian :: (Ord a) => Vector a -> a
sortedVectorMedian v = v ! ((V.length v - 1) `quot` 2)


medianByVectorHeapSort :: (Ord a) => [a] -> a
medianByVectorHeapSort = sortedVectorMedian . V.modify Heap.sort . V.fromList

medianByVectorMergeSort :: (Ord a) => [a] -> a
medianByVectorMergeSort = sortedVectorMedian . V.modify Merge.sort . V.fromList

medianByVectorIntroSort :: (Ord a) => [a] -> a
medianByVectorIntroSort = sortedVectorMedian . V.modify Intro.sort . V.fromList


tests = do
  quickCheck $ \(l :: [Int]) -> length l > 1 ==> medianSpecializedTo5 l == medianBySort l
  quickCheck $ \(l :: [Int]) -> length l > 1 ==> medianOfMedians l == medianBySort l
  quickCheck $ \(l :: [Int]) -> length l > 1 ==> forAll (choose (0, length l - 1)) $ \i -> select i l == sort l !! i

  quickCheck $ \(l :: [Int]) -> length l > 1 ==> medianByVectorHeapSort l == medianBySort l
  quickCheck $ \(l :: [Int]) -> length l > 1 ==> medianByVectorMergeSort l == medianBySort l
  quickCheck $ \(l :: [Int]) -> length l > 1 ==> medianByVectorIntroSort l == medianBySort l


main = do
  tests

  defaultMain
    [ bgroup (show n)
        [ bench "medianOfMedians" $ whnf medianOfMedians inputList
        , bench "medianBySort" $ whnf medianBySort inputList
        , bench "medianByHeapSortVector" $ whnf medianByVectorHeapSort inputList
        , bench "medianByMergeSortVector" $ whnf medianByVectorMergeSort inputList
        , bench "medianByIntroSortVector" $ whnf medianByVectorIntroSort inputList

        , bench "select" $ whnf (select (n - 1)) inputList
        , bench "selectBySort" $ whnf (selectBySort (n - 1)) inputList
        ]
    | power <- [0,1..4]
    , let n = (2 ^ power) * 1000
    , let inputList :: [Int]
          inputList = take n (randoms (mkStdGen 0))
    ]
