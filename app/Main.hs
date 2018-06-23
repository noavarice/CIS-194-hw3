module Main where

import Lib

main :: IO ()
main = putStrLn $ histogram list
  where
    list = [0, 2, 2, 1, 2, 1, 1, 2, 3, 5, 9, 8, 8, 8, 0]
