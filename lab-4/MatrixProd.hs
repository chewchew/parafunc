module Main where

import Control.Monad.Par
import Criterion.Main

type Matrix = [[Int]]
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

matrixVectorProd :: Matrix -> Vector -> Par Vector
matrixVectorProd m v = parMap (\ mv -> vectorVectorProd mv v ) (transpose m)

matrixProd :: Matrix -> Matrix -> Par Matrix
matrixProd lm rm = parMap (\ lmr -> matrixVectorProdSeq rm lmr) lm

matrixVectorProdSeq :: Matrix -> Vector -> Vector
matrixVectorProdSeq m v = map (\ mv -> vectorVectorProd mv v ) (transpose m)

matrixProdSeq :: Matrix -> Matrix -> Matrix
matrixProdSeq lm rm = map (\ lmr -> matrixVectorProdSeq rm lmr) lm

main :: IO()
main = do
    -- defaultMain [
    --     bench "matrixProd" (nf (runPar . matrixProd bigMatrix) bigMatrix),
    --     bench "matrixProdSeq" (nf (matrixProdSeq bigMatrix) bigMatrix)]
    defaultMain [
        bench "matrixProd" (nf (runPar . matrixProd bigMatrix) bigMatrix)]
    -- print $ head $ runPar (matrixProd bigMatrix bigMatrix)
