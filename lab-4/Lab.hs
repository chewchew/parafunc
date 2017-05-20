module Lab where

-- import qualified Data.Array.Repa as R
import Data.Array.Repa
import Control.Monad.Identity

type IndexValue = (Int,Int)

getI :: IndexValue -> Int
getI = fst

getV :: IndexValue -> Int
getV = snd

test :: Array U DIM1 Int
test = fromListUnboxed (Z :. 10) [1..10]

ins :: Array U DIM1 Int
ins = fromListUnboxed (Z :. 8) [0,0,2,9,8,10,1,10]

-- bestCand :: (Int,Int) -> (Int,Int) -> (Int,Int)
-- bestCand (x1,y1) (x2,y2) = 
--     if x1 < x2
--         then if y1 > y2
--             then (x1,y1)
--             else (x1,y2)
--         else if x1 - y1 > x2 - y2
--             then (x1,y1)
--             else (x2,y2)

-- buySell :: [(IndexValue,IndexValue)] -> (Int,Int,Int)
-- buySell prices = (min,max,max - min)
--     where 
--         (min,max) = foldl (\ c1 c2 -> bestCand c1 c2 ) (head prices) (tail prices)

diff :: (IndexValue,IndexValue) -> Int
diff (iv1,iv2) = getV iv2 - getV iv1

bestCand :: (IndexValue,IndexValue) -> (IndexValue,IndexValue) -> (IndexValue,IndexValue)
bestCand (x1,y1) (x2,y2) = foldl 
    (\ c1 c2 -> if diff c1 >= diff c2 then c1 else c2) 
    (x1,y1) [(x1,x2),(x1,y2),(y1,x2),(y1,y2),(x2,y2)]
               
buySell :: Array U DIM1 (IndexValue,IndexValue) -> (IndexValue,IndexValue)
buySell prices = head . toList . runIdentity $ foldP bestCand (prices ! (Z :. 0)) prices

toArray :: [(IndexValue,IndexValue)] -> Array U DIM1 (IndexValue,IndexValue)
toArray prices = fromListUnboxed (Z :. length prices) prices

makePairs :: [Int] -> [(IndexValue,IndexValue)]
makePairs xs = makePairs' [(i,x) | (i,x) <- zip [0..length xs - 1] xs]

makePairs' :: [IndexValue] -> [(IndexValue,IndexValue)]
makePairs' [x,y]    = [(x,y)]
makePairs' (x:y:xs) = (x,y) : makePairs' xs

main = do
    print $ test
    print $ toUnboxed test
    print $ ins
    print $ buySell $ toArray $ makePairs [0,0,2,9,8,10,1,10]