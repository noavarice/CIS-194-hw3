module Lib
    ( skips
    , localMaxima
    ) where

-- 1. Hopscotch

nthHelper :: [a] -> Int -> Int -> [a]
nthHelper [] _ _ = []
nthHelper (x:xs) n cur =
  if mod cur n == 0
    then x : next
    else next
  where
    next = nthHelper xs n (cur + 1)

nth :: [a] -> Int -> [a]
nth [] _ = []
nth list n = nthHelper list n 1

skips :: [a] -> [[a]]
skips [] = []
skips [x] = [[x]]
skips list = list : map (nth list) [2..length list]

-- 2. Local maxima
localMaximaHelper :: Integer -> Integer -> [Integer] -> [Integer]
localMaximaHelper prev curr [next] =
  if curr > prev && curr > next
    then [curr]
    else []
localMaximaHelper prev curr (next:remaining) =
  if curr > prev && curr > next
    then curr : others
    else others
  where
    others = localMaximaHelper curr next remaining

localMaxima :: [Integer] -> [Integer]
localMaxima list@(prev:curr:xs) =
  if length list < 3
    then []
    else localMaximaHelper prev curr xs
