module Lib
    ( skips
    ) where

nthHelper :: [a] -> Int -> Int -> [a]
nthHelper [] _ _ = []
nthHelper list n cur =
  if mod cur n == 0
    then (head list) : next
    else next
  where
    next = nthHelper (tail list) n (cur + 1)

nth :: [a] -> Int -> [a]
nth [] _ = []
nth list n = nthHelper list n 1

skips :: [a] -> [[a]]
skips [] = []
skips [x] = [[x]]
skips list = list : map (nth list) [2..length list]
