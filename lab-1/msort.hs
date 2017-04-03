module Main where

import Data.List
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Environment (getArgs)
import System.Random (StdGen, getStdGen, randomRs)

import Control.Parallel -- par and pseq (should be in base)
import Control.Parallel.Strategies

import Criterion.Main

force :: [a] -> ()
force [] = ()
force (x:xs) = x `pseq` force xs

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

cmp :: [Int] -> [Int] -> Bool
cmp [] [] = True
cmp xs [] = False
cmp [] ys = False
cmp (x:xs) (y:ys) = x == y && cmp xs ys

benchmark :: IO()
benchmark = do
    let n = 100000
    let d1 = 4
    let d2 = 4
    input1 <- randomInts n (1,10000) `fmap` getStdGen
    input2 <- randomInts n (1,10000) `fmap` getStdGen
    input3 <- randomInts n (1,10000) `fmap` getStdGen

    -- Run the sorting algorithms --
    let l1 = "pSort1 (seq,pseq) (d = " ++ show d1 ++ ")"
    let l2 = "pSort2 (Strategies,parTuple2,rdeepseq) (d = " ++ show d2 ++ ")"
    defaultMain 
        [
            bench l1 (nf (pSort1 d1) input1),
            bench l2 (nf (pSort2 d2) input2),
            bench "mSort (regular)" (nf mSort input3)
        ]

main :: IO()
main = benchmark
    -- let n = 10000
    -- input <- randomInts n (1,10000) `fmap` getStdGen
    -- print $ (pSort1 4 input) == (sort input)