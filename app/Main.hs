module Main where

import Lib

main :: IO ()
main = print $ localMaxima [1, 4, 3]
