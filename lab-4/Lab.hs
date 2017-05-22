module Main where

-- import qualified Data.Array.Repa as R
import System.Random
import Data.Array.Repa
import Control.Monad.Identity
import Criterion.Main

type IndexValue = (Int,Int)

getI :: IndexValue -> Int
getI = fst

getV :: IndexValue -> Int
getV = snd

test :: Array U DIM1 Int
test = fromListUnboxed (Z :. 10) [1..10]

ins :: Array U DIM1 Int
ins = fromListUnboxed (Z :. 8) [0,0,2,9,8,10,1,10]

diff :: (IndexValue,IndexValue) -> Int
diff (iv1,iv2) = getV iv2 - getV iv1

bestCand :: (IndexValue,IndexValue) -> (IndexValue,IndexValue) -> (IndexValue,IndexValue)
bestCand (x1,y1) (x2,y2) = foldl 
    (\ c1 c2 -> if diff c1 >= diff c2 then c1 else c2) 
    (x1,y1) [(x1,x2),(x1,y2),(y1,x2),(y1,y2),(x2,y2)]
{-# INLINE bestCand #-}

buySellS :: Array U DIM1 (IndexValue,IndexValue) -> (IndexValue,IndexValue)
buySellS prices = head . toList $ 
    foldS bestCand (prices ! (Z :. 0)) (extract (Z :. 1) len prices)
    where len = extent prices

buySellP :: Array U DIM1 (IndexValue,IndexValue) -> (IndexValue,IndexValue)
buySellP prices = head . toList . runIdentity $ 
    foldP bestCand (prices ! (Z :. 0)) (extract (Z :. 1) len prices)
    where len = extent prices

toArray :: [(IndexValue,IndexValue)] -> Array U DIM1 (IndexValue,IndexValue)
toArray prices = fromListUnboxed (Z :. length prices) prices

makePairs :: [Int] -> [(IndexValue,IndexValue)]
makePairs xs = makePairs' [(i,x) | (i,x) <- zip [0..length xs - 1] xs]

makePairs' :: [IndexValue] -> [(IndexValue,IndexValue)]
makePairs' [x,y]    = [(x,y)]
makePairs' (x:y:xs) = (x,y) : makePairs' xs

randomList :: Int -> Int -> IO [Int]
randomList a b = getStdGen >>= return . randomRs (a,b)

main :: IO()
main = do
    let n = 100000
    ints <- randomList 0 2000
    defaultMain [
        bench "foldS" (nf buySellS (toArray $ makePairs (take n ints))),
        bench "foldP" (nf buySellP (toArray $ makePairs (take n ints)))]