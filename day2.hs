--  Haskell code to solve the problem presented in Advent of Code 2016, day 2.
--  Author: Bob Wall
module AOCDay2 where

import Data.Char
import Data.List

type KeyPosition = (Int, Int)   --  row of the keypad (0 to 2) followed by column (0 to 2)
                                --  key 1 is (0,0), key 9 is (2, 2)

posToKey :: KeyPosition -> Char
posToKey pos = chr(ord('0') + row * 3 + col + 1)
  where
    row = fst pos
    col = snd pos

startPos :: KeyPosition
startPos = (1, 1)  -- the 5 key

getNextPos :: KeyPosition -> Char -> KeyPosition
getNextPos (0,   col) 'U' = (0,       col)
getNextPos (row, col) 'U' = (row - 1, col)
getNextPos (2,   col) 'D' = (2,       col)
getNextPos (row, col) 'D' = (row + 1, col)
getNextPos (row, 0)   'L' = (row,     0)
getNextPos (row, col) 'L' = (row,     col - 1)
getNextPos (row, 2)   'R' = (row,     2)
getNextPos (row, col) 'R' = (row,     col + 1)


solution2 :: (KeyPosition -> Char) -> (KeyPosition -> Char -> KeyPosition) -> KeyPosition -> [String] -> String
solution2 posToKeyFunc nextPosFunc startPos strs =
  reverse (fst (foldl' foldFunc ([], startPos) strs))
  where
    foldFunc :: (String, KeyPosition) -> String -> (String, KeyPosition)
    foldFunc acc str =
      ((posToKeyFunc nextPos) : (fst acc), nextPos)
      where
        innerFoldFunc :: KeyPosition -> String -> KeyPosition
        innerFoldFunc pos str = foldl' nextPosFunc pos str
        nextPos = innerFoldFunc (snd acc) str

solution2p1 :: [String] -> String
solution2p1 strs =
   solution2 posToKey getNextPos startPos strs


--  Second part keypad is a little different. Still use (row, col), but layout is
--        1
--      2 3 4
--    5 6 7 8 9
--      A B C
--        D
startPos' = (2, 0)  -- the 5 key

posToKey' :: KeyPosition -> Char
posToKey' (0, 2) = '1'
posToKey' (1, 1) = '2'
posToKey' (1, 2) = '3'
posToKey' (1, 3) = '4'
posToKey' (2, 0) = '5'
posToKey' (2, 1) = '6'
posToKey' (2, 2) = '7'
posToKey' (2, 3) = '8'
posToKey' (2, 4) = '9'
posToKey' (3, 1) = 'A'
posToKey' (3, 2) = 'B'
posToKey' (3, 3) = 'C'
posToKey' (4, 2) = 'D'

getNextPos' :: KeyPosition -> Char -> KeyPosition
getNextPos' (0,   2)   'U' = (0,       2)   -- 1 key
getNextPos' (0,   2)   'L' = (0,       2)
getNextPos' (0,   2)   'R' = (0,       2)
getNextPos' (1,   1)   'U' = (1,       1)   -- 2 key
getNextPos' (1,   1)   'L' = (1,       1)
getNextPos' (1,   3)   'U' = (1,       3)   -- 4 key
getNextPos' (1,   3)   'R' = (1,       3)
getNextPos' (2,   0)   'U' = (2,       0)   -- 5 key
getNextPos' (2,   0)   'L' = (2,       0)
getNextPos' (2,   0)   'D' = (2,       0)
getNextPos' (2,   4)   'U' = (2,       4)   -- 9 key
getNextPos' (2,   4)   'R' = (2,       4)
getNextPos' (2,   4)   'D' = (2,       4)
getNextPos' (3,   1)   'L' = (3,       1)   -- A key
getNextPos' (3,   1)   'D' = (3,       1)
getNextPos' (3,   3)   'R' = (3,       3)   -- C key
getNextPos' (3,   3)   'D' = (3,       3)
getNextPos' (4,   2)   'L' = (4,       2)   -- D key
getNextPos' (4,   2)   'D' = (4,       2)
getNextPos' (4,   2)   'R' = (4,       2)
getNextPos' (row, col) 'U' = (row - 1, col)
getNextPos' (row, col) 'D' = (row + 1, col)
getNextPos' (row, col) 'L' = (row,     col - 1)
getNextPos' (row, col) 'R' = (row,     col + 1)


solution2p2 :: [String] -> String
solution2p2 strs =
   solution2 posToKey' getNextPos' startPos' strs


printDay2Solutions :: IO ()
printDay2Solutions = do
  content <- readFile "day2.in"
  let moves = lines content
  print (show (solution2p1 moves))
  print (show (solution2p2 moves))
