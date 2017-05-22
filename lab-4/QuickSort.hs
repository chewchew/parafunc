module Main where

import Data.List
import Control.Monad.Par
import System.Random
import Criterion.Main

rlist :: StdGen -> Int -> [Int]
rlist g 0 = [fst $ randomR (0,1000) g]
rlist g i = x : rlist g' (i-1)
    where (x,g') = randomR (0,1000) g

-- Sequential Quicksort
qsortSeq :: [Int] -> [Int]
qsortSeq []     = []
qsortSeq (p:xs) = lt ++ [p] ++ gt
    where
        lt = qsortSeq (filter (<=p) xs)
        gt = qsortSeq (filter (>p)  xs)

-- Parallel Quicksort
qsortPar :: Int -> [Int] -> Par [Int]
qsortPar 0 xs     = return $ qsortSeq xs
qsortPar _ []     = return []
qsortPar k (p:xs) = do
    ilt <- spawn $ qsortPar (k-1) (filter (<=p) xs)
    igt <- spawn $ qsortPar (k-1) (filter (>p)  xs)
    lt <- get ilt
    gt <- get igt
    return $ lt ++ [p] ++ gt

main = do
    let n  = 100000
    let k  = 4
    let xs = rlist (mkStdGen 0) n
    defaultMain [
        bench "qsortSeq" (nf qsortSeq (rlist (mkStdGen 0) n)),
        bench "qsortPar" (nf (runPar . qsortPar k) (rlist (mkStdGen 0) n))]