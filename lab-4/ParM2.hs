module Main where

import Control.Monad
import Control.Monad.Par

type Matrix = [[Int]]
type Vector = [Int]

force :: Matrix -> Matrix
force m = map (map $ seq ()) m

identityMatrix3 :: Matrix
identityMatrix3 = [[1,0,0],[0,1,0],[0,0,1]]

testMatrix1 :: Matrix
testMatrix1 = force [
    [1,2,3],
    [4,5,6],
    [7,8,9]]

bigMatrix :: Matrix
bigMatrix = force [[1..500] | _ <- [1..100]]

transposeRow :: Vector -> Matrix
transposeRow v = [[x] | x <- v]

transpose :: Matrix -> Matrix
transpose (r:[]) = transposeRow r
transpose (r:m) = [l ++ r | (l,r) <- zip rt (transpose m)]
    where rt = transposeRow r

parMap' :: NFData b => (a -> b) -> [a] -> Par [b]
parMap' f as = do
    ibs <- mapM (spawn . return . f) as
    mapM get ibs

vectorVectorProd :: Vector -> Vector -> Int
vectorVectorProd v1 v2 = sum [x1 * x2 | (x1,x2) <- zip v1 v2]

matrixVectorProd :: Matrix -> Vector -> Vector
matrixVectorProd m v = map (\ mv -> vectorVectorProd mv v ) (transpose m)

matrixProd :: Matrix -> Matrix -> Par Matrix
matrixProd lm rm = do
    ivars <- sequence [new | _ <- lm]
    _ <- mapM (\ (ivar,row) -> 
        fork $ put ivar (matrixVectorProd rm row)) (zip ivars lm)
    mapM get ivars

test :: Int -> Par Int
test x = do
    [i1,i2] <- sequence [new,new]
    fork $ put i1 (x+1)
    fork $ put i2 (x+2)
    v1 <- get i1
    v2 <- get i2
    return $ v1 + v2

main :: IO()
main = do
    -- print $ head $ runPar (matrixProd testMatrix1 testMatrix1)
    print $ runPar (test 1)