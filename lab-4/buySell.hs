{-

   type Elem = {{buy_idx,sell_idx,profit},{min_idx,min_value},{cur_idx,cur_val}}
   fold on [Elem]
   init = {{0,0,0.0},{idx_1,val_1},{cur_idx,cur_val}}
   Associativity:
   a ´f´ (b `f` c) == (a ´f´ b) ´f´ c
   [a,b,c]
   b < a < c
   if cur_val - min_val > best_profit then
    replace buySellProfit
   if cur_val < min_val then
    replace minValIdx
   
   Assoc. Right:

   b ´f´ c:
   b = {0,0,0},{a_idx,a_val},{b_idx,b_val}
   c = {0,0,0}, {a_idx,a_val}, {c_idx,c_val}
   b_val - a_val < 0 since b < a so but b in min container and keep buySellProfit as is
   b = {0,0,0}, {b_idx,b_val} ,...
   c_val - a_val > 0 so swap buySellProfit put keep min cont as is
   c = {a_idx,c_idx,c_val - a_val}, ...
   compare b and c
   c_val - b_val > c_val - a_val so swap buySellProfit
   b_val < a_val so keep min container
   c' = {b_idx, c_idx, c_val - b_val}, {b_idx,b_val}, {c_idx_c_val}

   a ´f´ c':
   a = {0,0,0}, {a_idx,a_val}, {a_idx,a_val}
   c' = {b_idx,c_idx,c_val - b_val}, {b}, {c}
   adjust a but no chancge
   adjust c' but no change
   compare a and c'
   no change o return c'

   Assoc Left:
   a ´f´ b:
   adjust a but no change
   adjust b but no change
   compare a and b
   b_val - a_val < 0 so keep buySellProfit
   b_val < a_val so take bs min container
   res = {0,0,0},{b_idx,b-val}, {b_idx,b_val}

   res ´f´ c: 
   see Assoc. right
-}

import Data.Array.Repa

type Point = (Int,Int)
type Min = Point
type Max = Point
type BuySell = (Int,Int,Int)
type StockPoint = (BuySell,Min,Max)

toRepaArray :: [Point] ->  Array U DIM1 Point
toRepaArray ls = fromListUnboxed (Z :. (length ls)) ls 

toStockPoints :: [Point] -> Array D DIM1 StockPoint
toStockPoints ls@(h:ts) = Data.Array.Repa.map convert (toRepaArray ls) 
  where
    convert :: Point -> StockPoint
    convert m = ((0,0,0),h,m)

toPoints :: [Int] -> [Point]
toPoints ls = zip [0..lastIdx] ls
  where
    lastIdx = length ls - 1

mergePoints

main :: IO ()
main = putStrLn .show $
  toList $
    (toStockPoints . toPoints) [2..10]
