[meatballgraph]: ./dataflowmeatballgraph.png "Data Flow Graph"
[qsort]: ./dfqsort.png "Data Flow Qsort"
[matrix]: ./dfmatrixprod.png "Data Flow Matrix"

## Using the Par Monad for parallel Haskell programming
A great way to write parallel Haskell program is to utilize the Par Monad. This library avoids some problems with lazy evaluations in parallel and is very explicit regarding granularity and data dependencies. If a solution to your problem can be described using a data flow graph, look no further.

### Building parallelism with (kn)IVars and forks
Here is a small example of consuming meatballs in parallel
````haskell
finishPlate :: [Meatballs] -> TastedGood
finishPlate mbs = runPar $ do
  let (leftSplit,rightSplit) = splitAt (length mbs ´div´ 2) mbs
  resultA <- new
  resultB <- new
  fork (put resultA (consume leftSplit))
  fork (put resultB (consume rightSplit))
  aIsOk <- get resultA
  bIsOk <- get resultB
  return (aIsOk and bIsOk)
````
We are spawning to parallel computations to determine wether or not the meatballs tasted good 
and then returning the result using `IVar`'s. 

`IVar`'s can be loosely be descirbed as write-once variables used to communicate
values between parallel computations. In order to get parallel computations, we
need to use a fork. 

Here are some basic functions that can help us with our cutlery:
````haskell
  new :: Par (IVar a)                       -- create a new IVar
  put :: NFDATA a => IVar a -> a -> Par ()  -- put the value in the supplied IVar
  get :: IVar a -> Par a                    -- get the value from an IVar 
  fork :: Par () -> Par ()                  -- fork a computation to happen in parallel
  runPar :: Par a -> a                      -- run a parallel computation and return it's result
````

Two important things to know about the above functions is that `put` evaluates the
value to normal form and `get` only retrives a value from the IVar if there is
a value to retrive. 

Since `put` evaluates the value to normal form we don't have
to worry about forcing an evaluation to normal form. This is why we need a to be an instance of `NFDATA`. Only evaluating to weak-head normal form is something that can easily prevent speedups when parallelizing in Haskell with all the lazy evaluation that's going on.  

Due to the fact that the `get` function only retrievs a value from an IVar if there is a value to retrive we can get parallelism! We can wait until a value has been put into the IVar from somewhere (maybe from a parallel computation, wink wink). 

A great way to visualize the parallelism, created using the Par Monad, is a data flow graph. Every `fork` yields a new node and `get` connects the parent node with *forked* nodes.

![alt text][meatballgraph]

So this is what the parallelism looks like!

### the Par monad in the wild
Lets explore two examples of the Par Monad, quicksort and matrix multiplication. Quicksort has a nice divide and conquer property that works well in the context of data flow graphs. Matrix multiplication is solved with a simple map in parallel.

#### Quicksort
Quicksort, quicksort, quicksort... We have spent to much time together. But lets have one more go at each other. 

Quicksort's divide and concuer solution is nicely described with a data flow graph so we should really try this out with the Par Monad. Basically we are spawning the recursive calls to quicksort in parallel, but only to a certain depth. 

The `spawn`function is provided by the Par Monad API and the definition looks a little something like this:

```haskell
spawn :: NFData a => Par a -> Par (IVar a)
spawn p = do
  i <- new
  fork (do
    x <- p
    put i x)
  return i
```
This function takes a Par a, evaluates the result fully in a forked node and puts it in an IVar which is returned. Basically it's spawning a computation in parallel. This is used in combination with a sequential quicksort function to spin up a web of parallel quicksorting.

```haskell
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
```
We recursivley quicksort in parallel to a certain depth. We do this because we don't want each parallel comutation to have to little work. By doing this we get a speedup of 1.65 compared to running the sequential quicksort. This is on a 4-core machine so the speedup isn't great but it's there.  

Here is a visualization of the parallel computation with a data flow graph.
![alt text][qsort]
We used a depth of 4 but in this graph the depth is 2. However itis there to give some sorrt of representation of what we're doing in parallel. Recursively calling quciksort on sublists.

#### Matrix multiplication
Here is antoher example of parallel Haskell using the Par Monad. In this case it's not a typical divide and conquer solution. We are going to show you parallel matrix multiplication in Haskell.

Our solution uses the built in function `parMap` to map matrix vector multiplaction over a bunch of columns. Remember `spawn`? Not the 90's movie but the function defined above. **:smirking-face:** This function is used by `parMap` to fork new parallel computations.

````haskell
parMap :: NFData b => (a -> Par b) -> [a] -> Par [b]
parMap f ls = do
  is <- mapM (spawn . return . f) ls
  mapM get is
````
The chaining of `spawn . return . f` is just to get the types to work with `mapM`. We end up with a list of `IVar`'s and we the we simply map `get` on all of them to retrive the final values.

Now to the work of parallelizing matrix mulitplication! 

```haskell

module Main where

import Control.Monad.Par
import Criterion.Main

type Matrix = [Vector]
type Vector = [Int]

bigMatrix :: Matrix
bigMatrix = [[1..500] | _ <- [1..100]]

transposeRow :: Vector -> Matrix
transposeRow v = [[x] | x <- v]

transpose :: Matrix -> Matrix
transpose (r:[]) = transposeRow r
transpose (r:m) = [l ++ r | (l,r) <- zip rt (transpose m)]
    where rt = transposeRow r

vectorVectorProd :: Vector -> Vector -> Int
vectorVectorProd v1 v2 = sum [x1 * x2 | (x1,x2) <- zip v1 v2]

matrixVectorProd :: Matrix -> Vector -> Vector
matrixVectorProd m v = map (\ mv -> vectorVectorProd mv v ) (transpose m)

makeChunks :: Int -> Matrix -> [[Vector]]
makeChunks chunkSize [] = []
makeChunks chunkSize m  = take chunkSize m : makeChunks chunkSize (drop chunkSize m)

matrixProdPar :: Int -> Matrix -> Matrix -> Matrix
matrixProdPar chunkSize lm rm = concat . runPar $ parMap 
    (\rows -> matrixProdPar' rm rows) (makeChunks chunkSize lm)

matrixProdPar' :: Matrix -> [Vector] -> Matrix
matrixProdPar' m rows = map (\ row -> matrixVectorProd m row) rows

matrixProdSeq :: Matrix -> Matrix -> Matrix
matrixProdSeq lm rm = map (\ lmr -> matrixVectorProd rm lmr) lm

main :: IO()
main = do
    defaultMain [
        bench "matrixProdPar" (nf (matrixProdPar 10 bigMatrix) bigMatrix),
        bench "matrixProdSeq" (nf (matrixProdSeq bigMatrix) bigMatrix)]
```
Here we are creating chunks of rows in one matrix to be multiplied with each column in the other matrix. Each of these chunks is one parallel computation spawned in parMap. The speedup up we got from running this matrix multiplication in parallel, compared to the sequential version, is 1.82 on a 4-core machine. 

![alt text][matrix]

Here is the parallelism described by a data flow graph.

### The Par Monad is A-ok
We have showed som simple examples of how to parallelize in Haskell using the Par Monad. It's has a really nice API and you can get a really nice representation of the prallelism using data flow graphs. It also eliminates some of the problems of lazy evaluation when diagnosing performance and you are given a lot of control as a programmer.

But as a famous politician once said: *I don't stand by anything* 
