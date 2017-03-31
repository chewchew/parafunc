import Data.List
import System.Random
import Criterion.Main
import Control.Parallel
import Control.Parallel.Strategies
import Control.DeepSeq

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
parMap2 :: Int -> (a -> b) -> [a] -> [b]
parMap2 _ _ [] = []
parMap2 0 f ls = map f ls
parMap2 d f ls =  
  let splits   = splitAt (((length ls) + 1) `div` 2) ls
      r        = fst splits
      l        = snd splits
      pR       = parMap2 (d-1) f r
      pL       = parMap2 (d-1) f l
  in  par pR (pL ++ pR)

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


jackknife :: (NFData a, NFData b) => ([a] -> b) -> [a] -> [b]
jackknife f = parMap4 4 f . resamples 500


crud = zipWith (\x a -> sin (x / 300)**2 + a) [0..]

main = do
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
         bench "jackknife" (nf (jackknife  mean) rs)
         ]

