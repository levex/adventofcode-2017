module Lib
    ( checksumThis
    , secondChecksumThis
    , findDiv
    ) where

import Data.List.Split
import Data.String.Utils

checksumLine :: [Int] -> Int
checksumLine line
  = maximum line - minimum line

checksumThis :: String -> IO Int
checksumThis fn = do
  file <- readFile fn
  let all = (map (map read) $ map splitWs $ lines file)
  print (all :: [[Int]])
  return $ foldl (\sum l -> sum + checksumLine l) 0 all

isInt x = x == fromInteger (round x)

findDiv :: [Int] -> Int
findDiv xs
  = snd $ foldl (\(b, s) e -> if b then (b, s) else findDiv' xs e) (False, 0) xs
  where
    findDiv' :: [Int] -> Int -> (Bool, Int)
    findDiv' (y : ys) x
      | y == x         = findDiv' ys x
      | x `mod` y == 0 = (True, x `div` y)
      | otherwise      = findDiv' ys x
    findDiv' [] _
      = (False, 1)

secondChecksumThis :: String -> IO Int
secondChecksumThis fn = do
  file <- readFile fn
  let all = (map (map read) $ map splitWs $ lines file)
  return $ foldl (\sum l -> sum + findDiv l) 0 all
