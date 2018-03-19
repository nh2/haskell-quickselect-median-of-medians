-- TODO: Implement it for vectors, pull-request it into vector-algorithms

-- Based on http://austinrochford.com/posts/2013-10-29-median-of-medians-in-haskell.html
-- (which is slightly wrong because it can't deal with repeated elements,
-- see https://github.com/AustinRochford/blog-content/commit/ab32c7ab1ca1cd1555b87ec4edbeecbf03cad170#commitcomment-22433560

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

{-# LANGUAGE Strict #-}

{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE PartialTypeSignatures #-}

module Main (main) where

import           Control.DeepSeq (force, deepseq)
import           Control.Exception (evaluate)
import           Control.Monad (when)
import           Control.Monad.Primitive (PrimMonad, PrimState)
import           Control.Monad.ST (runST)
import           Control.Loop (forLoop)
import           Data.Foldable (for_)
import           Data.List (sort, sortOn)
import           Data.Traversable (for)
import           Data.Vector (Vector, (!))
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Generic as VG
import           Data.Vector.Generic.Mutable (MVector)
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Algorithms.Heap as Heap
import qualified Data.Vector.Algorithms.Merge as Merge
import qualified Data.Vector.Algorithms.Intro as Intro
import           GHC.Exts (build, inline)
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
  | otherwise =
      let
        pivot = medianOfMedians (map medianSpecializedTo5 (chunksOf 5 xs))
        (smaller, equal, larger) = pivotPartition xs pivot
        k = length smaller
        e = length equal
      in if
        | k <= i && i < k+e -> pivot
        | i < k             -> select i           smaller
        | otherwise         -> select (i - k - e) larger


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

    a_smaller_count :: Int
    a_smaller_count =  ab + ac + ad + ae
    b_smaller_count :: Int
    b_smaller_count = -ab + bc + bd + be
    c_smaller_count :: Int
    c_smaller_count = -ac - bc + cd + ce
    d_smaller_count :: Int
    d_smaller_count = -ad - bd - cd + de
    e_smaller_count :: Int
    e_smaller_count = -ae - be - ce - de


sortedVectorMedian :: (Ord a) => Vector a -> a
sortedVectorMedian v = v ! (medianIndex (V.length v))


medianByVectorHeapSort :: (Ord a) => [a] -> a
medianByVectorHeapSort = sortedVectorMedian . V.modify Heap.sort . V.fromList

medianByVectorMergeSort :: (Ord a) => [a] -> a
medianByVectorMergeSort = sortedVectorMedian . V.modify Merge.sort . V.fromList

medianByVectorIntroSort :: (Ord a) => [a] -> a
medianByVectorIntroSort = sortedVectorMedian . V.modify Intro.sort . V.fromList


-- TODO pass in the vector to make it slightly faster
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

    a_smaller_count :: Int
    a_smaller_count =  ab + ac + ad + ae
    b_smaller_count :: Int
    b_smaller_count = -ab + bc + bd + be
    c_smaller_count :: Int
    c_smaller_count = -ac - bc + cd + ce
    d_smaller_count :: Int
    d_smaller_count = -ad - bd - cd + de
    e_smaller_count :: Int
    e_smaller_count = -ae - be - ce - de

thirdSmallestOf5WithIndicesCounts :: (Ord a) => Int -> a -> Int -> a -> Int -> a -> Int -> a -> Int -> a -> (Int, Int, Int, Int, Int)
thirdSmallestOf5WithIndicesCounts ai a bi b ci c di d ei e =
  (a_smaller_count, b_smaller_count, c_smaller_count, d_smaller_count, e_smaller_count)
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

    a_smaller_count :: Int
    a_smaller_count =  ab + ac + ad + ae
    b_smaller_count :: Int
    b_smaller_count = -ab + bc + bd + be
    c_smaller_count :: Int
    c_smaller_count = -ac - bc + cd + ce
    d_smaller_count :: Int
    d_smaller_count = -ad - bd - cd + de
    e_smaller_count :: Int
    e_smaller_count = -ae - be - ce - de


nthSmallestOf5WithIndices :: (Ord a) => Int -> Int -> a -> Int -> a -> Int -> a -> Int -> a -> Int -> a -> Int
nthSmallestOf5WithIndices n ai a bi b ci c di d ei e
  | a_smaller_count == checkVal = ai
  | b_smaller_count == checkVal = bi
  | c_smaller_count == checkVal = ci
  | d_smaller_count == checkVal = di
  | e_smaller_count == checkVal = ei
  | otherwise = error "nthSmallestOf5WithIndices: cannot happen"
  where
    checkVal = case n of
      0 ->  4
      1 ->  2
      2 ->  0
      3 -> -2
      4 -> -4

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

    a_smaller_count :: Int
    a_smaller_count =  ab + ac + ad + ae
    b_smaller_count :: Int
    b_smaller_count = -ab + bc + bd + be
    c_smaller_count :: Int
    c_smaller_count = -ac - bc + cd + ce
    d_smaller_count :: Int
    d_smaller_count = -ad - bd - cd + de
    e_smaller_count :: Int
    e_smaller_count = -ae - be - ce - de


thirdSmallestOf4WithIndices :: (Ord a) => Int -> a -> Int -> a -> Int -> a -> Int -> a -> Int
thirdSmallestOf4WithIndices ai a bi b ci c di d
  | a_smaller_count == 1 = ai
  | b_smaller_count == 1 = bi
  | c_smaller_count == 1 = ci
  | d_smaller_count == 1 = di
  | otherwise = error "thirdSmallestOf4WithIndices: cannot happen"
  where
    ab = if a < b then 1 else -1
    ac = if a < c then 1 else -1
    ad = if a < d then 1 else -1

    bc = if b < c then 1 else -1
    bd = if b < d then 1 else -1

    cd = if c < d then 1 else -1

    a_smaller_count :: Int
    a_smaller_count =  ab + ac + ad
    b_smaller_count :: Int
    b_smaller_count = -ab + bc + bd
    c_smaller_count :: Int
    c_smaller_count = -ac - bc + cd
    d_smaller_count :: Int
    d_smaller_count = -ad - bd - cd

nthSmallestOf4WithIndices :: (Ord a) => Int -> Int -> a -> Int -> a -> Int -> a -> Int -> a -> Int
nthSmallestOf4WithIndices n ai a bi b ci c di d
  | a_smaller_count == checkVal = ai
  | b_smaller_count == checkVal = bi
  | c_smaller_count == checkVal = ci
  | d_smaller_count == checkVal = di
  | otherwise = error "nthSmallestOf4WithIndices: cannot happen"
  where
    checkVal = case n of
      0 ->  3
      1 ->  1
      2 -> -1
      3 -> -3

    ab = if a < b then 1 else -1
    ac = if a < c then 1 else -1
    ad = if a < d then 1 else -1

    bc = if b < c then 1 else -1
    bd = if b < d then 1 else -1

    cd = if c < d then 1 else -1

    a_smaller_count :: Int
    a_smaller_count =  ab + ac + ad
    b_smaller_count :: Int
    b_smaller_count = -ab + bc + bd
    c_smaller_count :: Int
    c_smaller_count = -ac - bc + cd
    d_smaller_count :: Int
    d_smaller_count = -ad - bd - cd


secondSmallestOf3WithIndices :: (Ord a) => Int -> a -> Int -> a -> Int -> a -> Int
secondSmallestOf3WithIndices ai a bi b ci c
  | a_smaller_count == 0 = ai
  | b_smaller_count == 0 = bi
  | c_smaller_count == 0 = ci
  | otherwise = error "secondSmallestOf3WithIndices: cannot happen"
  where
    ab = if a < b then 1 else -1
    ac = if a < c then 1 else -1

    bc = if b < c then 1 else -1

    a_smaller_count :: Int
    a_smaller_count =  ab + ac
    b_smaller_count :: Int
    b_smaller_count = -ab + bc
    c_smaller_count :: Int
    c_smaller_count = -ac - bc

nthSmallestOf3WithIndices :: (Ord a) => Int -> Int -> a -> Int -> a -> Int -> a -> Int
nthSmallestOf3WithIndices n ai a bi b ci c
  | a_smaller_count == checkVal = ai
  | b_smaller_count == checkVal = bi
  | c_smaller_count == checkVal = ci
  | otherwise = error "nthSmallestOf3WithIndices: cannot happen"
  where
    checkVal = case n of
      0 ->  2
      1 ->  0
      2 -> -2

    ab = if a < b then 1 else -1
    ac = if a < c then 1 else -1

    bc = if b < c then 1 else -1

    a_smaller_count :: Int
    a_smaller_count =  ab + ac
    b_smaller_count :: Int
    b_smaller_count = -ab + bc
    c_smaller_count :: Int
    c_smaller_count = -ac - bc


-- TODO remove
myread :: (PrimMonad m, MVector v e, Ord e, Show e) => v (PrimState m) e -> Int -> m e
myread v i
  | i < 0 || i >= VGM.length v = error $ "myread: out of bounds: " ++ show i ++ " " ++ show (0 :: Int, VGM.length v)
  | otherwise = inline (VGM.unsafeRead v i)

{-# INLINE myswap #-}
myswap :: (PrimMonad m, MVector v e, Ord e, Show e) => v (PrimState m) e -> Int -> Int -> m ()
-- myswap !v !i !j = VGM.unsafeSwap v i j
myswap !v !i !j = do
  x <- inline (VGM.unsafeRead v i)
  y <- inline (VGM.unsafeRead v j)--
  inline (VGM.unsafeWrite v i y)
  inline (VGM.unsafeWrite v i x)

-- {-# NOINLINE myunboxed #-}
-- myunboxed :: (PrimMonad m, Ord e, Show e, VUM.Unbox e ) => VUM.MVector (PrimState m) e -> Int -> Int -> m ()
-- myunboxed v i j = do
--   x <- VUM.unsafeRead v i
--   y <- VUM.unsafeRead v j
--   VUM.unsafeWrite v i y
--   VUM.unsafeWrite v i x

medianOfMediansVector :: (VG.Vector v e, Ord e, Show e) => v e -> e
medianOfMediansVector v = selectVector (medianIndex (VG.length v)) v

selectVector :: (VG.Vector v e, Ord e, Show e) => Int -> v e -> e
selectVector i v = runST $ do
  vm <- VG.thaw v -- copies
  -- selectVectorDestructive i v vm
  -- selectVectorDestructive2 i v vm 0 (VGM.length vm)
  error "not used" -- TODO recomment above line
  myread vm 0 -- TODO ensure (length v > 0)

sliceBeginInclusiveEndExclusive :: (MVector vm e, Ord e, Show e) => Int -> Int -> vm s e -> vm s e
sliceBeginInclusiveEndExclusive beginInclusive endExclusive v = VGM.unsafeSlice beginInclusive (endExclusive - beginInclusive) v

-- TODO test that it's stable

-- Places the selected element at the beginning of the vector.
-- selectVectorDestructive :: forall m v e . (PrimMonad m, MVector v e, Ord e, Show e) => Int -> v (PrimState m) e -> m ()
selectVectorDestructive :: forall m v vm e . (PrimMonad m, VG.Vector v e, MVector vm e, Ord e, Show e, VG.Mutable v ~ vm) => Int -> v e -> vm (PrimState m) e -> m ()
selectVectorDestructive i x v = traceShow ("selectVectorDestructive on length", VGM.length v) $ do

  for_ [0..lastChunk] $ \c -> do -- TODO use `loop`
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
      _ -> trace "base case 1" $ do
        indicesValues <- for [0..chunkSize-1] $ \o -> fmap (c*5 + o, ) (v `myread` (c*5 + o)) -- TODO don't use lists
        return $ fst (sortOn snd indicesValues !! medianIndex chunkSize)
    myswap v c chunkMedianIndex

  if (n <= 5)
    then trace "base case 2" $ do
      values <- for [0..n-1] (v `myread`) -- TODO ugly
      let (_, selectedIndex) = select i (zip values [(0::Int)..])
      myswap v 0 selectedIndex

    else do
      traceShow "subMedian" $ destructiveMedian (sliceBeginInclusiveEndExclusive 0 numChunks v)
      pivot <- v `myread` 0

      -- let partitionLoop :: Int -> Int -> Int -> m Int
      --     partitionLoop !k !endIndex !equalCount = do
      --      if

      --       | k > endIndex -> {-# SCC "k_bigger_endIndex" #-} do
      --           -- The element at position (k-1) is <= pivot, so we can swap it with the pivot.
      --           -- Write pivot into the "middle" (pivotTargetPosition)
      --           let pivotTargetPosition = k - 1
      --           myswap v 0 pivotTargetPosition
      --           return pivotTargetPosition
      --       | otherwise -> {-# SCC "not_k_bigger_endIndex" #-} do
      --           x <- v `myread` k
      --           if
      --             | x > pivot -> {-# SCC "x_bigger_pivot" #-} do
      --                 myswap v k endIndex
      --                 partitionLoop k (endIndex - 1) equalCount
      --             | x < pivot -> {-# SCC "x_smaller_pivot" #-} do
      --                 partitionLoop (k + 1) endIndex equalCount
      --             -- x == pivot
      --             -- We put equal elements alternately into the left and right side,
      --             -- so that even for repeated elements equal to the median, the
      --             -- partition is still nicely balanced, guaranteeing O(n) run time
      --             -- even in that case.
      --             | equalCount `rem` 2 == 0 -> {-# SCC "x_eq_pivot_1" #-}   partitionLoop (k + 1) endIndex       (equalCount + 1)
      --             | otherwise               -> {-# SCC "x_eq_pivot_2" #-} do myswap v k endIndex
      --                                                                        partitionLoop k       (endIndex - 1) (equalCount + 1)

      -- pivotIndex <- traceShow ("pivot of length", VGM.length v, pivot) $ partitionLoop 1 (n - 1) 0
      pivotIndex <- traceShow ("pivot of length", VGM.length v, pivot) $ partitionLoop v pivot 1 (n - 1) 0

      if
        | pivotIndex == i -> myswap v 0 pivotIndex -- v[pivotIndex] is the sought element; swap it into the front
        | i < pivotIndex -> do
            selectVectorDestructive i                    x (sliceBeginInclusiveEndExclusive 0                pivotIndex v)
        | otherwise      -> do
            selectVectorDestructive (i - pivotIndex - 1) x (sliceBeginInclusiveEndExclusive (pivotIndex + 1) n          v)
            myswap v 0 (pivotIndex + 1)

  return ()

  where
    n = VGM.length v
    numChunks = (n+4) `quot` 5
    lastChunk = numChunks - 1
    dividesPerfectly = n `rem` 5 == 0

    -- TODO re-type
    -- destructiveMedian :: (PrimMonad m, MVector vm e, Ord e, Show e) => vm (PrimState m) e -> m ()
    destructiveMedian v = selectVectorDestructive (medianIndex (VGM.length v)) x v

-- {-# INLINE selectVectorDestructive2 #-}
-- {-# NOINLINE selectVectorDestructive2 #-}
--                                                                                                  TODO: `VG.Mutable v ~ vm` Makes it 10x slower!
-- selectVectorDestructive2 :: forall m v vm e . (PrimMonad m, VG.Vector v e, MVector vm e, Ord e, Show e, VG.Mutable v ~ vm) => Int -> v e -> vm (PrimState m) e -> Int -> Int -> m ()
-- selectVectorDestructive2 :: Int -> VU.Vector Int -> VUM.MVector (PrimState IO) Int -> Int -> Int -> IO ()
-- selectVectorDestructive2 :: forall m v vm e . (PrimMonad m, VG.Vector v e, MVector vm e, Ord e, Show e, VG.Mutable v ~ vm) => Int -> v e -> vm (PrimState m) e -> Int -> Int -> m () -- slow
-- selectVectorDestructive2 :: forall m v vm e . (PrimMonad m, VG.Vector v e, MVector vm e, Ord e, Show e) => Int -> v e -> vm (PrimState m) e -> Int -> Int -> m () -- fast
-- selectVectorDestructive2 :: (PrimMonad m, VG.Vector v e, Ord e, Show e, MVector vm e, VG.Mutable v ~ vm) => Int -> v e -> vm (PrimState m) e -> Int -> Int -> m () -- slow

-- ISSUE IS HERE:
--
-- Using
--     VG.Mutable v ~ vm => ... ->           vm (PrimState m) e -> ...
-- makes everything 10x slower than using
--     ...               => ... -> VG.Mutable v (PrimState m) e -> ...

-- slow
selectVectorDestructive2 :: (PrimMonad m, VG.Vector v e, Ord e, Show e, VG.Mutable v ~ vm) => Int -> v e -> vm (PrimState m) e -> Int -> Int -> m ()
-- fast
-- selectVectorDestructive2 :: (PrimMonad m, VG.Vector v e, Ord e, Show e) => Int -> v e -> VG.Mutable v (PrimState m) e -> Int -> Int -> m ()
selectVectorDestructive2 !i !x !v !begin !endExcl = traceShow ("selectVectorDestructive2 on length", n, "begin", begin, "endExcl", endExcl) $ do

  for_ [0..lastChunk] $ \c -> do -- TODO use `loop`
  -- forLoop 0 (<= lastChunk) (+1) $ \c -> do
    let chunkSize
          | c == lastChunk && not dividesPerfectly = n `rem` 5
          | otherwise                              = 5
    !chunkMedianIndex <- case chunkSize of
      -- This happens only at the very end of the vector and if is length isn't divisible by 5
      5 -> do
        e0 <- v `myread` (begin + c*5 + 0)
        e1 <- v `myread` (begin + c*5 + 1)
        e2 <- v `myread` (begin + c*5 + 2)
        e3 <- v `myread` (begin + c*5 + 3)
        e4 <- v `myread` (begin + c*5 + 4)
        return $ thirdSmallestOf5WithIndices (c*5 + 0) e0 (c*5 + 1) e1 (c*5 + 2) e2 (c*5 + 3) e3 (c*5 + 4) e4
      4 -> do
        -- TODO Possibly ensure that this gives the same as the _ case below
        e0 <- v `myread` (begin + c*5 + 0)
        e1 <- v `myread` (begin + c*5 + 1)
        e2 <- v `myread` (begin + c*5 + 2)
        e3 <- v `myread` (begin + c*5 + 3)
        return $ thirdSmallestOf4WithIndices (c*5 + 0) e0 (c*5 + 1) e1 (c*5 + 2) e2 (c*5 + 3) e3
      3 -> do
        e0 <- v `myread` (begin + c*5 + 0)
        e1 <- v `myread` (begin + c*5 + 1)
        e2 <- v `myread` (begin + c*5 + 2)
        return $ secondSmallestOf3WithIndices (c*5 + 0) e0 (c*5 + 1) e1 (c*5 + 2) e2
      _ -> do
        -- TODO Possibly ensure that this gives the same as the _ case below
        return $ c*5 + 0
      -- _ -> trace "base case 1" $ do
      --   indicesValues <- for [0..chunkSize-1] $ \o -> fmap (c*5 + o, ) (v `myread` (begin + c*5 + o)) -- TODO don't use lists
      --   return $ fst (sortOn snd indicesValues !! medianIndex chunkSize)
    myswap v (begin + c) (begin + chunkMedianIndex)

  if (n <= 5)
    then trace "base case 2" $ do
      -- values <- for [0..n-1] (\o -> v `myread` (begin + o)) -- TODO ugly
      -- let (_, selectedIndex) = select i (zip values [(0::Int)..])
      !selectedIndex <- case n of
        5 -> do
          e0 <- v `myread` (begin + 0)
          e1 <- v `myread` (begin + 1)
          e2 <- v `myread` (begin + 2)
          e3 <- v `myread` (begin + 3)
          e4 <- v `myread` (begin + 4)
          return $ nthSmallestOf5WithIndices i (0) e0 (1) e1 (2) e2 (3) e3 (4) e4
        4 -> do
          -- TODO Possibly ensure that this gives the same as the _ case below
          e0 <- v `myread` (begin + 0)
          e1 <- v `myread` (begin + 1)
          e2 <- v `myread` (begin + 2)
          e3 <- v `myread` (begin + 3)
          return $ nthSmallestOf4WithIndices i (0) e0 (1) e1 (2) e2 (3) e3
        3 -> do
          e0 <- v `myread` (begin + 0)
          e1 <- v `myread` (begin + 1)
          e2 <- v `myread` (begin + 2)
          return $ nthSmallestOf3WithIndices i (0) e0 (1) e1 (2) e2
        2 -> do
          e0 <- v `myread` (begin + 0)
          e1 <- v `myread` (begin + 1)
          return $ if
            -- TODO Possibly ensure that this gives the same as the _ case below
            | e0 < e1 && i == 0 -> 0
            | e1 < e0 && i == 1 -> 0
            | otherwise         -> 1
        1 -> return 0
        -- _ -> do
        --   values <- for [0..n-1] (\o -> v `myread` (begin + o)) -- TODO ugly
        --   let (_, selectedIndex) = select i (zip values [(0::Int)..])
        --   return selectedIndex

      myswap v (begin + 0) (begin + selectedIndex)

    else do
      traceShow "subMedian" $ destructiveMedian v begin (begin + numChunks)
      pivot <- v `myread` begin

      pivotIndex <- traceShow ("partitioning on length", n, "pivot", pivot) $ partitionLoop2 v begin pivot (begin + 1) (endExcl - 1) 0

      if
        | pivotIndex == i -> myswap v begin (begin + pivotIndex) -- v[pivotIndex] is the sought element; swap it into the front
        | i < pivotIndex -> do
            selectVectorDestructive2 i                    x v begin (begin + pivotIndex)
        | otherwise      -> do
            selectVectorDestructive2 (i - pivotIndex - 1) x v (begin + pivotIndex + 1) endExcl
            myswap v begin (begin + pivotIndex + 1)

  return ()

  where
    n = endExcl - begin
    numChunks = (n+4) `quot` 5
    lastChunk = numChunks - 1
    dividesPerfectly = n `rem` 5 == 0

    -- TODO re-type
    -- destructiveMedian :: (PrimMonad m, MVector vm e, Ord e, Show e) => vm (PrimState m) e -> m ()
    destructiveMedian !v !beg !en = selectVectorDestructive2 (medianIndex (en - beg)) x v beg en


partitionLoop :: (PrimMonad m, MVector vm e, Ord e, Show e) => vm (PrimState m) e -> e -> Int -> Int -> Int -> m Int
partitionLoop v !pivot !k !endIndex !equalCount = do
 if
  | k > endIndex -> {-# SCC "k_bigger_endIndex" #-} do
      -- The element at position (k-1) is <= pivot, so we can swap it with the pivot.
      -- Write pivot into the "middle" (pivotTargetPosition)
      let pivotTargetPosition = k - 1
      myswap v 0 pivotTargetPosition
      return pivotTargetPosition
  -- | otherwise -> {-# SCC "not_k_bigger_endIndex" #-} do
  | otherwise -> do
      x <- v `myread` k
      if
        -- | x > pivot -> {-# SCC "x_bigger_pivot" #-} do
        | x > pivot -> do
            myswap v k endIndex
            partitionLoop v pivot k (endIndex - 1) equalCount
        | x < pivot -> {-# SCC "x_smaller_pivot" #-} do
            partitionLoop v pivot (k + 1) endIndex equalCount
        -- x == pivot
        -- We put equal elements alternately into the left and right side,
        -- so that even for repeated elements equal to the median, the
        -- partition is still nicely balanced, guaranteeing O(n) run time
        -- even in that case.
        | equalCount `rem` 2 == 0 -> {-# SCC "x_eq_pivot_1" #-}   partitionLoop v pivot (k + 1) endIndex       (equalCount + 1)
        | otherwise               -> {-# SCC "x_eq_pivot_2" #-} do myswap v k endIndex
                                                                   partitionLoop v pivot k       (endIndex - 1) (equalCount + 1)


{-# INLINE partitionLoop2 #-}
partitionLoop2 :: (PrimMonad m, MVector vm e, Ord e, Show e) => vm (PrimState m) e -> Int -> e -> Int -> Int -> Int -> m Int
-- partitionLoop2 :: VUM.MVector (PrimState IO) Int -> Int -> Int -> Int -> Int -> Int -> IO Int
-- partitionLoop2 :: (PrimMonad m, MVector vm e, Ord e, Show e) => vm (PrimState m) e -> Int -> e -> Int -> Int -> Int -> m Int
partitionLoop2 v !begin !pivot !k !endIndex !equalCount = do
 if
  | k > endIndex -> {-# SCC "k_bigger_endIndex" #-} do
      -- The element at position (k-1) is <= pivot, so we can swap it with the pivot.
      -- Write pivot into the "middle" (pivotTargetPosition)
      let pivotTargetPosition = k - 1
      myswap v begin pivotTargetPosition
      return (pivotTargetPosition - begin)
  -- | otherwise -> {-# SCC "not_k_bigger_endIndex" #-} do
  | otherwise -> do
      x <- v `myread` k
      if
        -- | x > pivot -> {-# SCC "x_bigger_pivot" #-} do
        | x > pivot -> do
            myswap v k endIndex
            partitionLoop2 v begin pivot k (endIndex - 1) equalCount
        | x < pivot -> {-# SCC "x_smaller_pivot" #-} do
            partitionLoop2 v begin pivot (k + 1) endIndex equalCount
        -- x == pivot
        -- We put equal elements alternately into the left and right side,
        -- so that even for repeated elements equal to the median, the
        -- partition is still nicely balanced, guaranteeing O(n) run time
        -- even in that case.
        | equalCount `rem` 2 == 0 -> {-# SCC "x_eq_pivot_1" #-}   partitionLoop2 v begin pivot (k + 1) endIndex       (equalCount + 1)
        | otherwise               -> {-# SCC "x_eq_pivot_2" #-} do myswap v k endIndex
                                                                   partitionLoop2 v begin pivot k       (endIndex - 1) (equalCount + 1)


tests = do
  check $ \(l :: [Int]) -> length l > 1 ==> medianSpecializedTo5 l == medianBySort l
  check $ \(l :: [Int]) -> length l > 1 ==> medianOfMedians l == medianBySort l
  check $ \(l :: [Int]) -> length l > 1 ==> forAll (choose (0, length l - 1)) $ \i -> select i l == sort l !! i

  check $ \(l :: [Int]) -> length l > 1 ==> medianByVectorHeapSort l == medianBySort l
  check $ \(l :: [Int]) -> length l > 1 ==> medianByVectorMergeSort l == medianBySort l
  check $ \(l :: [Int]) -> length l > 1 ==> medianByVectorIntroSort l == medianBySort l

  check $ \(l :: [Int]) -> length l > 1 ==> medianOfMediansVector (V.fromList l) == medianBySort l

  -- check $ \(a :: Int, b, c, d, e) -> thirdSmallestOf5WithIndices 0 a 1 b 2 c 3 d 4 e == snd (sort (zip [a, b, c, d, e] [0..]) !! 2)
  -- check $ \(a :: Int, b, c, d) -> thirdSmallestOf4WithIndices 0 a 1 b 2 c 3 d == snd (sort (zip [a, b, c, d] [0..]) !! 2)
  where
    check :: (Arbitrary a, Show a, Testable t) => (a -> t) -> IO ()
    check = quickCheckWith stdArgs { maxSuccess = 1000 }

main = do
  -- tests

  -- mv <- VG.thaw (VU.fromList [(1::Int)..100])

  -- for_ [0..1000000::Int] $ \i -> do
  --   -- x <- VGM.unsafeRead mv 0
  --   -- VGM.unsafeWrite mv 0 (x + 5)
  --   myswap mv 0 1

  let v = VU.fromList $ concat $ replicate 10 [(1::Int)..100]
  mv <- VG.thaw v

  -- for_ [(1::Int)..1000] $ \i -> do
  for_ [(1::Int)..100] $ \i -> do
    -- selectVectorDestructive 0 v mv
    selectVectorDestructive2 0 v mv 0 (VGM.length mv)

  -- for_ [(1::Int)..10000] $ \i -> do
  --   partitionLoop mv 0 0 999 0

  -- defaultMain
  --   [ bgroup (show n)
  --       [ bench "medianOfMedians" $ whnf medianOfMedians inputList
  --       , bench "medianBySort" $ whnf medianBySort inputList
  --       , bench "medianByHeapSortVector" $ whnf medianByVectorHeapSort inputList
  --       , bench "medianByMergeSortVector" $ whnf medianByVectorMergeSort inputList
  --       , bench "medianByIntroSortVector" $ whnf medianByVectorIntroSort inputList

  --       , bench "select" $ whnf (select (n - 1)) inputList
  --       , bench "selectBySort" $ whnf (selectBySort (n - 1)) inputList

  --       , bench "medianOfMediansVector" $ whnf (medianOfMediansVector) (VU.fromList inputList)

  --       -- , bench "thirdSmallestOf5" $ whnf (thirdSmallestOf5 1 2 3 4) 5
  --       -- , bench "thirdSmallestOf5WithIndices" $ whnf (thirdSmallestOf5WithIndices 1 1 2 2 3 3 4 4 5) 5
  --       ]
  --   | power :: Int <- [0,1..4]
  --   , let n = (2 ^ power) * 1000
  --   , let inputList :: [Int]
  --         inputList = take n (randoms (mkStdGen 0))
  --   ]

  -- let v2 = VU.fromList $ concat $ replicate 10 [(1::Int)..100]
  -- mv2 <- VU.thaw v2
  -- myunboxed mv2 1 2
