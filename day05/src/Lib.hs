module Lib
    ( solveThis
    , partOne
    , partTwo
    ) where

import qualified Data.Sequence as S
import Data.Foldable

partOne :: [Int] -> Int
partOne ls
  = solve' ls 0 0
  where
    solve' :: [Int] -> Int -> Int -> Int
    solve' ls n s
      | n >= length ls || n < 0 = s
      | otherwise     = solve' (Data.Foldable.foldr (:) [] ls') (n + curr) (s + 1)
      where
        curr = ls !! n
        ls' = (S.update n (curr + 1) $ S.fromList ls)

partTwo :: [Int] -> Int
partTwo ls
  = solve' ls 0 0
  where
    solve' :: [Int] -> Int -> Int -> Int
    solve' ls n s
      | n >= length ls || n < 0 = s
      | otherwise     = solve' (Data.Foldable.foldr (:) [] ls') (n + curr) (s + 1)
      where
        curr = ls !! n
        ls' = (S.update n (curr + v) $ S.fromList ls)
        v = if curr >= 3 then -1 else 1

solveThis :: ([Int] -> Int) -> String -> IO Int
solveThis part fn = do
  file <- readFile fn
  let ln = lines file
  let lns = (map read ln) :: [Int]
  return $ part lns
  
