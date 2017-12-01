module Lib
    ( calcCaptcha
    , doCaptcha
    , doHalfwayCaptcha
    , first
    , second
    ) where


conv '1' = 1
conv '2' = 2
conv '3' = 3
conv '4' = 4
conv '5' = 5
conv '6' = 6
conv '7' = 7
conv '8' = 8
conv '9' = 9
conv '0' = 0
conv _ = 0

-- given the string, go and do the captcha logic
doCaptcha :: String -> Int
doCaptcha str
  = snd $ foldl (\(lst, sum) e -> if e == lst then (e, sum + conv e) else (e, sum)) (last str, 0) str

doHalfwayCaptcha :: String -> Int
doHalfwayCaptcha str
  = snd $
    foldl (\(pos, sum) e ->
      if e == str !! ((pos + offset) `mod` length str)
        then (pos + 1, sum + conv e)
        else (pos + 1, sum))
    (0, 0) str
  where
    offset = (length str) `div` 2

first = doCaptcha
second = doHalfwayCaptcha

-- given a filename, returns the captcha value
calcCaptcha :: (String -> Int) -> String -> IO Int
calcCaptcha version fn = do
  str <- filter (\c -> c >= '0' && c <= '9') <$> readFile fn
  return $ version str
