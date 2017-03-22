import Data.List
import System.Random
import Criterion.Main
import Control.Parallel

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
-- This is currently crap performance but trying to figure out why
parMap2 :: Int -> (a -> b) -> [a] -> [b]
parMap2 _ _ []     = []
parMap2 d f ls = let xs = parMap2 d f $ drop d ls
                     x  = map f $ take d ls
                 in par x (pseq xs (x ++ xs))

jackknife :: ([a] -> b) -> [a] -> [b]
jackknife f = parMap2 10 f . resamples 500


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

