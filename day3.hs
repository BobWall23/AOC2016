--  Haskell code to solve the problem presented in Advent of Code 2016, day 3.
--  Author: Bob Wall
module AOCDay3 where

import Data.List

type Triangle = (Int, Int, Int)


parseLine :: String -> Triangle
parseLine line =
  (a, b, c)
  where
    ints = (fmap read . words) line
    a = ints !! 0
    b = ints !! 1
    c = ints !! 2


validTriangle :: Triangle -> Bool
validTriangle (a, b, c) = (a + b > c) && (a + c > b) && (b + c > a)


remapTriangles :: [Triangle] -> [Triangle]
remapTriangles triangles =
  go triangles []
  where
    go :: [Triangle] -> [Triangle] -> [Triangle]
    go [] soFar = soFar
    go ((a, b, c) : (d, e, f) : (g, h, i) : remaining) soFar =  go remaining ((a, d, g) : (b, e, h) : (c, f, i) : soFar)


solution3 :: [Triangle] -> Int
solution3 triangles =
  length (filter validTriangle triangles)


printDay3Solutions :: IO ()
printDay3Solutions = do
    content <- readFile "day3.in"
    let triangles = fmap parseLine (lines content)
    print $ "Total legal triangles: " ++ show (solution3 triangles)
    let newTriangles = remapTriangles triangles
    print $ "Total legal triangles: " ++ show (solution3 newTriangles)
