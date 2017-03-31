import Data.List
import System.Random
import Criterion.Main
import Control.Parallel
import Control.Parallel.Strategies
import Control.DeepSeq
import Control.Monad.Par
-- code borrowed from the Stanford Course 240h (Functional Systems in Haskell)
-- I suspect it comes from Bryan O'Sullivan, author of Criterion

data T a = T !a !Int


mean :: (RealFrac a) => [a] -> a
mean = fini . foldl' go (T 0 0)
  where
    fini (T a _) = a
    go (T m n) x = T m' n'
      where m' = m + (x - m) / fromIntegral n'
            n' = n + 1


resamples :: Int -> [a] -> [[a]]
resamples k xs =
    take (length xs - k) $
    zipWith (++) (inits xs) (map (drop k) (tails xs))

-- Parallel map which parallelize every f x
parMap1 :: (a -> b) -> [a] -> [b]
parMap1 _ []     = []
parMap1 f (l:ls) = let xs = parMap1 f ls
                       x  = f l
                   in par x (pseq xs (x:xs))

-- Parallel map like above but only parallelize to a certain depth
-- Don't know why this doesn't create parallelism
parMap2 :: (NFData a, NFData b) => Int -> (a -> b) -> [a] -> [b]
parMap2 _ _ [] = []
parMap2 0 f ls = map f ls
parMap2 d f ls =  
  let splits   = splitAt (((length ls) + 1) `div` 2) ls
      r        = fst splits
      l        = snd splits
      pR       = parMap2 (d-1) f r
      pL       = parMap2 (d-1) f l
  in  par (force pR) (pseq (force pL) (pL ++ pR))

-- Parallel map with Eval monad and rpar/rseq/rseq pattern
parMap3 :: (a -> b) -> [a] -> [b]
parMap3 _ [] = []
parMap3 f (l:ls) = runEval $ do
  a <- rpar (f l)
  b <- rseq (parMap3 f ls)
  rseq a
  return (a:b)

-- Parallel map with Eval monad and rpar/rseq/rseq pattern and
-- adjusted for more granularity control
parMap4 :: (NFData a, NFData b) => Int -> (a -> b) -> [a] -> [b]
parMap4 _ _ [] = []
parMap4 0 f ls = map f ls
parMap4 d f ls = runEval $ do
  let (l,r) = splitAt (((length ls) + 1) `div` 2) ls
  a <- rpar (force (parMap4 (d-1) f l))
  b <- rseq (force (parMap4 (d-1) f r))
  rseq a
  return (a ++ b)

parMap5 :: (NFData a, NFData b) => Int -> (a -> b) -> [a] -> [b]
parMap5 _ _ [] = []
parMap5 d f ls = runEval $ do
  let left = drop d ls 
  a <- rpar (force (map f (take d ls)))
  l <- rseq (force left)
  b <- rseq (force (parMap4 d f l))
  rseq a
  return (a ++ b)

parMap6 :: (NFData a, NFData b) => (a -> b) -> [a] -> [b]
parMap6 f ls = map f ls `using` parList rseq 

mySpawn :: NFData a => Par a -> Par (IVar a)
mySpawn p = do 
  i <- new
  fork (do
    x <- p
    put i x)
  return i

mParMap :: NFData b => (a -> Par b) -> [a] -> Par [b]
mParMap f ls = do
  is <- mapM (mySpawn . f) ls
  mapM get is

parMap7 :: (NFData a, NFData b) => (a -> b) -> [a] -> [b]
parMap7 f ls = runPar $ mParMap (return . f) ls


jackknife :: (NFData a, NFData b) => ([a] -> b) -> [a] -> [b]
jackknife f = map f . resamples 500

jackknifePar1 :: (NFData a, NFData b) => ([a] -> b) -> [a] -> [b]
jackknifePar1 f = parMap1 f . resamples 500

jackknifePar2 :: (NFData a, NFData b) => ([a] -> b) -> [a] -> [b]
jackknifePar2 f = parMap2 10 f . resamples 500

jackknifeStdParMap :: (NFData a, NFData b) => ([a] -> b) -> [a] -> [b]
jackknifeStdParMap f = Control.Parallel.Strategies.parMap rpar f . resamples 500

jackknifePar3 :: (NFData a, NFData b) => ([a] -> b) -> [a] -> [b]
jackknifePar3 f = parMap3 f . resamples 500

jackknifePar4 :: (NFData a, NFData b) => ([a] -> b) -> [a] -> [b]
jackknifePar4 f = parMap4 3 f . resamples 500

jackknifePar5 :: (NFData a, NFData b) => ([a] -> b) -> [a] -> [b]
jackknifePar5 f = parMap5 30 f . resamples 500

jackknifePar6 :: (NFData a, NFData b) => ([a] -> b) -> [a] -> [b]
jackknifePar6 f = parMap6 f . resamples 500

jackknifePar7 :: (NFData a, NFData b) => ([a] -> b) -> [a] -> [b]
jackknifePar7 f = parMap7 f . resamples 500

crud = zipWith (\x a -> sin (x / 300)**2 + a) [0..]

benchmarkIt = do
  let (xs,ys) = splitAt 1500  (take 6000
                               (randoms (mkStdGen 211570155)) :: [Float] )
  -- handy (later) to give same input different parallel functions

  let rs = crud xs ++ ys
  putStrLn $ "sample mean:    " ++ show (mean rs)

  let j = jackknife mean rs :: [Float]
  putStrLn $ "jack mean min:  " ++ show (minimum j)
  putStrLn $ "jack mean max:  " ++ show (maximum j)
  defaultMain
        [
         bench "jackknife sequential" (nf (jackknife mean) rs),
         bench "jackknife par and pseq" (nf (jackknifePar1 mean) rs),
         bench "jackknife par and pseq with depth 10 (splitting list)" (nf (jackknifePar1 mean) rs),
         bench "jackknife Standard parMap from parallel" (nf (jackknifeStdParMap mean) rs),
	 bench "jackknife Eval monad with rpar/rseq/rseq" (nf (jackknifePar3 mean) rs),
	 bench "jackknife rpar/rseq/rseq with depth" (nf (jackknifePar4 mean) rs),
         bench "jackknife rpar/rseq/rseq fixed chunks of work" (nf (jackknifePar5 mean) rs),
         bench "jackknife with Strategies" (nf (jackknifePar6 mean) rs),
         bench "jackknife with Par Monad" (nf (jackknifePar7 mean) rs)
        ]

run = do
  putStrLn "Running just one jackknife"
  let ls = jackknifePar7  mean (take 10000 (randoms (mkStdGen 211570155))::[Float])
  putStrLn $ "Result: " ++ (show ls)
  return ()

main = benchmarkIt
