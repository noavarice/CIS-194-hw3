module Main where

import Lib

main :: IO ()
main = print $ localMaxima [1, 2, 3, 4, 5]
