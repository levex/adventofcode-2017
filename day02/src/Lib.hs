module Lib
    ( checksumThis
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
