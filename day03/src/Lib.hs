module Lib
    ( partOne
    ) where

partOne :: Int -> Int
partOne input
  = abs x + abs y
  where
    (x, y) = partOne' 0 0 1 1 input

partOne' :: Int -> Int -> Int -> Int -> Int -> (Int, Int)
partOne' x y d n num
  | n' >= num     = (x, y' - n' + num)
  | n'' >= num   = (x' + n'' - num, y')
  | n''' >= num  = (x', y'' + n''' - num)
  | n'''' >= num = (x'' - n'''' + num, y'')
  | otherwise    = partOne' x'' y'' d'' n'''' num
  where
   y' = y + d
   n' = n + d -- 1
   x' = x - d
   n'' = n' + d -- 2
   d' = d + 1
   y'' = y' - d'
   n''' = n'' + d' -- 3
   x'' = x' + d'
   n'''' = n''' + d'
   d'' = d' + 1 -- 4
