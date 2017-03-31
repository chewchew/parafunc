module Main where

import Data.List
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Environment (getArgs)
import System.Random (StdGen, getStdGen, randomRs)

import Control.Parallel -- par and pseq (should be in base)
import Criterion.Main

--import Control.Monad.Par -- Par monad (monad-par on hackage)

force :: [a] -> ()
force [] = ()
force (x:xs) = x `pseq` force xs

randomInts :: Int -> (Int,Int) -> StdGen -> [Int]
randomInts k range g = let result = take k (randomRs range g)
                  in force result `pseq` result


split :: [Int] -> ([Int],[Int])
split xs = splitAt ((length xs + 1) `div` 2) xs

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
mSort xs 	  =
	let 
		(xs1,xs2) = split xs 
	in 
		merge (mSort xs1) (mSort xs2)

pSort :: Int -> [Int] -> [Int]
pSort d []      = []
pSort d (x:[])  = [x]
pSort d (x:[y]) = if x < y then [x,y] else [y,x]
pSort 0 xs      = mSort xs
pSort d xs 	    = 
	let 
		(xs1,xs2) = split xs
		s1 = pSort (d-1) xs1
		s2 = pSort (d-1) xs2
	in
		s1 `par` (s2 `pseq` (merge s1 s2))

main = do
	let d = 10

	--input <- randomInts 10000000 (1,1000000) `fmap` getStdGen
	--start <- getCurrentTime
	--seq (mSort input) (return ())
	--end   <- getCurrentTime
	--putStrLn $ "mSort: " ++ show (end `diffUTCTime` start)

	input <- randomInts 10000 (1,10000) `fmap` getStdGen
	--start <- getCurrentTime
	--seq (pSort d input) (return ())
	--end   <- getCurrentTime
	--putStrLn $ "pSort: " ++ show (end `diffUTCTime` start)

	-- print $ sort input == mSort input && mSort input == pSort d input

	defaultMain [ bench "msort" $ nf (pSort d) input ]