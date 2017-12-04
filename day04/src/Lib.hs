module Lib
    ( solve
    , partOne
    , partTwo
    ) where

import Data.List
import Data.List.Split
import Data.String.Utils

partOne :: String -> Bool
partOne str
  = nub str' == str'
  where
    str' = splitWs str

partTwo :: String -> Bool
partTwo str
  = nub v == v
  where
    str' = splitWs str
    v = sort $ map sort str'

solve :: (String -> Bool) -> String -> IO Int
solve for fn = do
  file <- readFile fn
  let ln = lines file
  return $ foldl (\n l -> if for l then n + 1 else n) 0 ln
