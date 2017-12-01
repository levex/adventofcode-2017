module Main where

import Lib
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  captcha <- calcCaptcha first (head args)
  print captcha
