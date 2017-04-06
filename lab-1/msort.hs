module Main where

import Data.List
import System.Random (StdGen, getStdGen, randomRs)

import Control.Parallel -- par and pseq (should be in base)
import Control.Parallel.Strategies
import Control.DeepSeq
import Criterion.Main

--force :: [a] -> ()
--force [] = ()
--force (x:xs) = x `pseq` force xs

randomInts :: Int -> (Int,Int) -> StdGen -> [Int]
randomInts k range g = let result = take k (randomRs range g)
                  in force result `pseq` result

split :: [Int] -> ([Int],[Int])
split xs = (take n xs, drop n xs)
    where n = (length xs + 1) `div` 2

-- mSort
-- Regular merge sort
merge :: [Int] -> [Int] -> [Int]
merge [] []     = []
merge xs []     = xs
merge [] ys     = ys
merge (x:xs) (y:ys) =
    if x < y
        then x : (merge xs (y:ys))
        else y : (merge (x:xs) ys)

mSort :: [Int] -> [Int]
mSort []      = []
mSort (x:[])  = [x]
mSort (x:[y]) = if x < y then [x,y] else [y,x]
mSort xs      =
    let 
        (xs1,xs2) = split xs 
    in 
        merge (mSort xs1) (mSort xs2)

-- pSort1
-- Parallelized with par and pseq
pSort1 :: Int -> [Int] -> [Int]
pSort1 d []    = []
pSort1 d [x]   = [x]
pSort1 d [x,y] = if x < y then [x,y] else [y,x]
pSort1 0 xs    = mSort xs
pSort1 d xs    = 
    let 
        (xs1,xs2) = split xs
        s1 = pSort1 (d-1) xs1
        s2 = pSort1 (d-1) xs2
    in
        (force s1) `par` (force s2) `pseq` (merge s1 s2)

-- pSort2
-- Parallelized with parTuple2 Strategy
pMerge :: ([Int],[Int]) -> [Int]
pMerge ([],[]) = []
pMerge (xs,[]) = xs
pMerge ([],ys) = ys
pMerge ((x:xs),(y:ys)) =
    if x < y
        then x : (pMerge (xs, (y:ys)))
        else y : (pMerge ((x:xs), ys))

pSort2 :: Int -> [Int] -> [Int]
pSort2 _ []    = []
pSort2 _ [x]   = [x]
pSort2 _ [x,y] = if x < y then [x,y] else [y,x]
pSort2 0 xs = mSort xs
pSort2 d xs = 
    let
        (xs1,xs2) = split xs
        s1 = pSort2 (d-1) xs1
        s2 = pSort2 (d-1) xs2
    in
        pMerge ((s1,s2) `using` parTuple2 rdeepseq rdeepseq)

mergesort2 :: [Int] -> [Int] 
mergesort2 xs = runEval $ go xs
  where
    go [] = return []
    go [x] = return [x]
    go [x,y] = if x < y then return [x,y] else return [y,x]
    go xs = do
      let (xs1,xs2) = split xs
      s1 <- go xs1
      s2 <- go xs2
      parList rdeepseq (pMerge (s1,s2))  

-- Run a benchmark on the sorting algorithms --
benchmark :: IO()
benchmark = do
    let n = 100000
    let d1 = 3
    let d2 = 2
    input1 <- randomInts n (1,10000) `fmap` getStdGen
    input2 <- randomInts n (1,10000) `fmap` getStdGen
    input3 <- randomInts n (1,10000) `fmap` getStdGen

    let l1 = "pSort1 (par,pseq) (d = " ++ show d1 ++ ")"
    let l2 = "pSort2 (Strategies,parTuple2,rdeepseq) (d = " ++ show d2 ++ ")"
    defaultMain 
        [
            bench l1 (nf (pSort1 d1) input1),
            bench l2 (nf (pSort2 d2) input2),
            bench "mSort (regular)" (nf mSort input3)
        ]

main :: IO()
main = benchmark
