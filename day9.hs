--  Haskell code to solve the problem presented in Advent of Code 2016, day 9.
--  Author: Bob Wall
module AOCDay9 where

import Data.List
import Data.Char


tailOrEmpty :: String -> String
tailOrEmpty "" = ""
tailOrEmpty str = tail str

readOrZero :: String -> Int
readOrZero "" = 0
readOrZero str = read (takeWhile isDigit str)

day9Solutionp1 :: String -> String
day9Solutionp1 "" = ""
day9Solutionp1 str =
  prefix ++ repeatStr ++ day9Solutionp1 rest
  where
    (prefix, suffix) = span (/= '(') str
    (next, afterNext) = span (/= ')') suffix
    repeat = tailOrEmpty next               -- Skip opening '('
    afterRepeat = tailOrEmpty afterNext     -- Skip opening ')'
    repeatLen = readOrZero (takeWhile isDigit repeat)
    repeatCt = readOrZero (tailOrEmpty (dropWhile (/= 'x') repeat))
    repeatStr = take (repeatLen * repeatCt) (cycle (take repeatLen afterRepeat))
    rest = drop repeatLen afterRepeat
         
day9Solutionp2 :: String -> Int
day9Solutionp2 "" = 0
day9Solutionp2 str =
  (length prefix) + totalRepeatLen + day9Solutionp2 rest
  where
    (prefix, suffix) = span (/= '(') str
    (next, afterNext) = span (/= ')') suffix
    repeat = tailOrEmpty next               -- Skip opening '('
    afterRepeat = tailOrEmpty afterNext     -- Skip opening ')'
    repeatLen = readOrZero (takeWhile isDigit repeat)
    repeatCt = readOrZero (tailOrEmpty (dropWhile (/= 'x') repeat))
    totalRepeatLen = repeatCt * (day9Solutionp2 (take repeatLen afterRepeat))
    rest = drop repeatLen afterRepeat
        

printDay9Solutions :: IO ()
printDay9Solutions = do
  content <- readFile "day9.in"
  let str = concat (lines content)
  print $ show (length (day9Solutionp1 str))
  print $ show (day9Solutionp2 str)
