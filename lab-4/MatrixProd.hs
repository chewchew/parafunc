module Main where

import Control.Monad.Par
import Criterion.Main

type Matrix = [Vector]
type Vector = [Int]

identityMatrix3 :: Matrix
identityMatrix3 = [[1,0,0],[0,1,0],[0,0,1]]

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

-- how it works
-- benchmarks
-- chunking
-- data flow
main :: IO()
main = do
    defaultMain [
        bench "matrixProdPar" (nf (matrixProdPar 20 bigMatrix) bigMatrix),
        bench "matrixProdSeq" (nf (matrixProdSeq bigMatrix) bigMatrix)]
    -- defaultMain [
    --     bench "matrixProdPar" (nf (matrixProdPar 10 bigMatrix) bigMatrix)]
    -- print $ makeChunks 1 testMatrix1
    -- print $ matrixProdPar 1 testMatrix1 testMatrix1
