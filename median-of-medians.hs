-- TODO: Implement it for vectors, pull-request it into vector-algorithms

-- Based on http://austinrochford.com/posts/2013-10-29-median-of-medians-in-haskell.html
-- (which is slightly wrong because it can't deal with repeated elements,
-- see https://github.com/AustinRochford/blog-content/commit/ab32c7ab1ca1cd1555b87ec4edbeecbf03cad170#commitcomment-22433560

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import           Control.DeepSeq (deepseq)
import           Control.Monad (when)
import           Control.Monad.Primitive (PrimMonad, PrimState)
import           Control.Monad.ST (runST)
import           Data.Foldable (for_)
import           Data.List (sort, partition, sortOn)
import           Data.Traversable (for)
import           Data.Tuple (swap)
import           Data.Vector (Vector, (!))
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import           Data.Vector.Generic.Mutable (MVector)
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Algorithms.Heap as Heap
import qualified Data.Vector.Algorithms.Merge as Merge
import qualified Data.Vector.Algorithms.Intro as Intro
import           GHC.Exts (build)
import           GHC.Stack (HasCallStack)
import           System.Random

import           Criterion.Main
import           Test.QuickCheck


import qualified Debug.Trace as Trace

traceEnabled = False

traceShow :: Show a => a -> b -> b
traceShow a b = if traceEnabled then Trace.traceShow a b else b

trace :: String -> a -> a
trace str a = if traceEnabled then Trace.trace str a else a


median :: (Ord a) => [a] -> a
median = medianOfMedians


medianIndex :: Int -> Int
medianIndex size = (size - 1) `quot` 2


medianOfMedians :: (Ord a) => [a] -> a
medianOfMedians xs = select (medianIndex (length xs)) xs


-- | Selects the i-th smallest element (i=0 is the smallest element).
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
medianBySort xs = selectBySort (medianIndex (length xs)) xs


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
sortedVectorMedian v = v ! (medianIndex (V.length v))


medianByVectorHeapSort :: (Ord a) => [a] -> a
medianByVectorHeapSort = sortedVectorMedian . V.modify Heap.sort . V.fromList

medianByVectorMergeSort :: (Ord a) => [a] -> a
medianByVectorMergeSort = sortedVectorMedian . V.modify Merge.sort . V.fromList

medianByVectorIntroSort :: (Ord a) => [a] -> a
medianByVectorIntroSort = sortedVectorMedian . V.modify Intro.sort . V.fromList


thirdSmallestOf5WithIndices :: (Ord a) => Int -> a -> Int -> a -> Int -> a -> Int -> a -> Int -> a -> Int
thirdSmallestOf5WithIndices ai a bi b ci c di d ei e
  | a_smaller_count == 0 = ai
  | b_smaller_count == 0 = bi
  | c_smaller_count == 0 = ci
  | d_smaller_count == 0 = di
  | e_smaller_count == 0 = ei
  | otherwise = error "thirdSmallestOf5WithIndices: cannot happen"
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


-- TODO remove
myread :: (HasCallStack, PrimMonad m, MVector v e, Ord e, Show e) => v (PrimState m) e -> Int -> m e
myread v i
  | i < 0 || i >= VGM.length v = error $ "myread: out of bounds: " ++ show i ++ " " ++ show (0, VGM.length v)
  | otherwise = VGM.read v i


medianOfMediansVector :: (VG.Vector v e, Ord e, Show e) => v e -> e
medianOfMediansVector v = selectVector (medianIndex (VG.length v)) v

selectVector :: (VG.Vector v e, Ord e, Show e) => Int -> v e -> e
selectVector i v = runST $ do
  vm <- VG.thaw v -- copies
  selectVectorDestructive i v vm
  myread vm 0 -- TODO ensure (length v > 0)

sliceBeginInclusiveEndExclusive :: (MVector vm e, Ord e, Show e) => Int -> Int -> vm s e -> vm s e
sliceBeginInclusiveEndExclusive beginInclusive endExclusive v = VGM.slice beginInclusive (endExclusive - beginInclusive) v

-- TODO test that it's stable

-- Places the selected element at the beginning of the vector.
-- selectVectorDestructive :: forall m v e . (PrimMonad m, MVector v e, Ord e, Show e) => Int -> v (PrimState m) e -> m ()
selectVectorDestructive :: forall m v vm e . (PrimMonad m, VG.Vector v e, MVector vm e, Ord e, Show e, VG.Mutable v ~ vm) => Int -> v e -> vm (PrimState m) e -> m ()
selectVectorDestructive i x v = do
  x' :: v e <- VG.freeze v

  traceShow (i, VG.toList x') for_ [0..lastChunk] $ \c -> do -- TODO use `loop`
    let chunkSize
          | c == lastChunk && not dividesPerfectly = n `rem` 5
          | otherwise                              = 5
    !chunkMedianIndex <- case chunkSize of
      -- This happens only at the very end of the vector and if is length isn't divisible by 5
      5 -> do
        e0 <- v `myread` (c*5 + 0)
        e1 <- v `myread` (c*5 + 1)
        e2 <- v `myread` (c*5 + 2)
        e3 <- v `myread` (c*5 + 3)
        e4 <- v `myread` (c*5 + 4)
        return $ thirdSmallestOf5WithIndices (c*5 + 0) e0 (c*5 + 1) e1 (c*5 + 2) e2 (c*5 + 3) e3 (c*5 + 4) e4
      _ -> do
        indicesValues <- for [0..chunkSize-1] $ \o -> fmap (c*5 + o, ) (v `myread` (c*5 + o)) -- TODO don't use lists
        return $ fst (sortOn snd indicesValues !! medianIndex chunkSize)

    traceShow ("chunkMedianIndex swap", c, chunkMedianIndex) VGM.swap v c chunkMedianIndex

  if (n <= 5)
    then do
      values <- for [0..n-1] (v `myread`) -- TODO ugly
      let (_, selectedIndex) = select i (zip values [(0::Int)..])
      traceShow ("n <= 5 final swap", 0, selectedIndex) VGM.swap v 0 selectedIndex

    else do
      destructiveMedian (sliceBeginInclusiveEndExclusive 0 numChunks v)
      xAfterPivotFind :: v e <- VG.freeze v
      pivot <- trace ("xAfterPivotFind " ++ show (VG.toList xAfterPivotFind)) $ v `myread` 0

      let partitionLoop :: Int -> Int -> Int -> m Int
          partitionLoop k endIndex equalCount = do
           xInPartitionLoop :: v e <- VG.freeze v
           trace ("partitionLoop " ++ show k ++ " " ++ show endIndex ++ " "++ show equalCount ++ " on " ++ show (VG.toList xInPartitionLoop) ++ " with pivot " ++ show pivot) $ if

            | k > endIndex -> do
                -- The element at position (k-1) is <= pivot, so we can swap it with the pivot.
                let middle = medianIndex n
                -- Write pivot into the "middle" (pivotTargetPosition)
                let pivotTargetPosition = k - 1
                traceShow ("swap pivot", 0, k-1) $ VGM.swap v 0 pivotTargetPosition
                return pivotTargetPosition
            | otherwise -> do
                x <- v `myread` k
                if
                  | x > pivot -> do
                      traceShow ("swap >", k, endIndex) $ VGM.swap v k endIndex
                      partitionLoop k (endIndex - 1) equalCount
                  | x < pivot -> do
                      partitionLoop (k + 1) endIndex equalCount
                  -- x == pivot
                  -- We put equal elements alternately into the left and right side,
                  -- so that even for repeated elements equal to the median, the
                  -- partition is still nicely balanced, guaranteeing O(n) run time
                  -- even in that case.
                  | equalCount `rem` 2 == 0 -> partitionLoop (k + 1) endIndex       (equalCount + 1)
                  | otherwise               -> do
                                                  traceShow ("swap >", k, endIndex) $ VGM.swap v k endIndex
                                                  partitionLoop k       (endIndex - 1) (equalCount + 1)

      pivotIndex <- partitionLoop 1 (n - 1) 0

      xAfterPart :: v e <- VG.freeze v

      trace ("final pivot position: " ++ show pivotIndex ++ " at with pivot value " ++ show pivot) $ trace ("xAfterPart " ++ show (VG.toList xAfterPart)) $ if
        | pivotIndex == i -> VGM.swap v 0 pivotIndex -- v[pivotIndex] is the sought element; swap it into the front
        | i < pivotIndex -> do
            trace "going left"  $ selectVectorDestructive i                    x (sliceBeginInclusiveEndExclusive 0                pivotIndex v)
        | otherwise      -> do
            trace "going right" $ selectVectorDestructive (i - pivotIndex - 1) x (sliceBeginInclusiveEndExclusive (pivotIndex + 1) n          v)
            traceShow ("swap after recurse right", 0, pivotIndex + 1) $ VGM.swap v 0 (pivotIndex + 1)

  return ()

  where
    n = VGM.length v
    numChunks = (n+4) `quot` 5
    lastChunk = numChunks - 1
    dividesPerfectly = n `rem` 5 == 0

    -- TODO re-type
    -- destructiveMedian :: (PrimMonad m, MVector vm e, Ord e, Show e) => vm (PrimState m) e -> m ()
    destructiveMedian v = selectVectorDestructive (medianIndex (VGM.length v)) x v


tests = do
  check $ \(l :: [Int]) -> length l > 1 ==> medianSpecializedTo5 l == medianBySort l
  check $ \(l :: [Int]) -> length l > 1 ==> medianOfMedians l == medianBySort l
  check $ \(l :: [Int]) -> length l > 1 ==> forAll (choose (0, length l - 1)) $ \i -> select i l == sort l !! i

  check $ \(l :: [Int]) -> length l > 1 ==> medianByVectorHeapSort l == medianBySort l
  check $ \(l :: [Int]) -> length l > 1 ==> medianByVectorMergeSort l == medianBySort l
  check $ \(l :: [Int]) -> length l > 1 ==> medianByVectorIntroSort l == medianBySort l

  check $ \(l :: [Int]) -> length l > 1 ==> medianOfMediansVector (V.fromList l) == medianBySort l
  where
    check = quickCheckWith stdArgs { maxSuccess = 1000 }

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
