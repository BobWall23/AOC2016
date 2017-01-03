--  Haskell code to solve the problem presented in Advent of Code 2016, day 6.
--  Author: Bob Wall
module AOCDay6 where

import Data.Char
import Data.List


countChar :: String -> Char -> Int
countChar str c = length (filter (== c) str)

getMostFrequent :: String -> Char
getMostFrequent str =
  snd (last (sort flist))
  where
    freqs = [countChar str c | c <- ['a'..'z']]
    flist = zip freqs ['a'..'z']

getLeastFrequent :: String -> Char
getLeastFrequent str =
  snd (head (dropWhile (\x -> (fst x) == 0) (sort flist)))
  where
    freqs = [countChar str c | c <- ['a'..'z']]
    flist = zip freqs ['a'..'z']


printDay6Solutions :: IO ()
printDay6Solutions = do
  content <- readFile "day6.in"
  let istrs = transpose (lines content)
  let mostFrequent = map getMostFrequent istrs
  print mostFrequent
  let leastFrequent = map getLeastFrequent istrs
  print leastFrequent
