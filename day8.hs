--  Haskell code to solve the problem presented in Advent of Code 2016, day 8.
--  Author: Bob Wall
module AOCDay8 where

import Data.Char
import Data.List
import Data.Array

type Panel = Array (Int, Int) Bool

width :: Int
width = 50
height :: Int
height = 6

panel :: Panel
panel = listArray ((0, 0), (height - 1, width - 1)) (replicate 1000 False)

rect :: Panel -> Int -> Int -> Panel
rect panel width height =
  panel // [ ((row, col) , True) | col <- [0 .. width - 1], row <- [0 .. height - 1] ]

rotateCol :: Panel -> Int -> Int -> Panel
rotateCol panel col dist =
  panel // rotCol
  where
    c = [ panel ! (row, col) | row <- [0 .. height - 1] ]
    amt = height - dist
    rotc = (drop amt c) ++ (take amt c)
    rotCol = zip [(row, col) | row <- [0 .. height - 1] ] rotc

rotateRow :: Panel -> Int -> Int -> Panel
rotateRow panel row dist =
  panel // rotRow
  where
    r = [ panel ! (row, col) | col <- [0 .. width - 1] ]
    amt = width - dist
    rotr = (drop amt r) ++ (take amt r)
    rotRow = zip [(row, col) | col <- [0 .. width - 1] ] rotr

cmd1 = "rect "
cmd2 = "rotate column x="
cmd3 = "rotate row y="

matchCommand :: String -> String -> String
matchCommand cmd inp = if ((take (length cmd) inp) == cmd) then (drop (length cmd) inp) else ""

parseRect :: String -> (Int, Int)
parseRect cmd =
  (w, h)
  where
    w = read (takeWhile isDigit cmd)
    h = read (tail (dropWhile isDigit cmd))

parseRotate :: String -> (Int, Int)
parseRotate cmd =
  (rOrC, dist)
  where
    rOrC = read (takeWhile isDigit cmd)
    dist = read (drop 4 (dropWhile isDigit cmd))  -- skip the " by "

applyCommand :: Panel -> String -> Panel
applyCommand p cmd =
  do
    let rest1 = matchCommand cmd1 cmd
    if (length rest1 > 0)
      then do
        let (w, h) = parseRect rest1
        rect p w h
      else do
        let rest2 = matchCommand cmd2 cmd
        if (length rest2 > 0)
          then do
            let (col, dist) = parseRotate rest2
            rotateCol p col dist
          else do
            let rest3 = matchCommand cmd3 cmd
            if (length rest3 > 0)
              then do
                let (row, dist) = parseRotate rest3
                rotateRow p row dist
              else p


toDisplay :: Panel -> [String]
toDisplay p = do
  breakStr [ if (p ! (r, c)) then '*' else ' ' | r <- [0 .. height - 1], c <- [0 .. width - 1] ] []
  where
    breakStr :: String -> [String] -> [String]
    breakStr [] strs = strs
    breakStr remaining strs =
       breakStr (drop width remaining) ((take width remaining) : strs)
  

printDay8Solutions :: IO ()
printDay8Solutions = do
  content <- readFile "day8.in"
  let cmds = lines content
  let p = foldl' applyCommand panel cmds
  let pList = [ p ! (r, c) | r <- [0..height - 1], c <- [0..width - 1] ]
  let numSet = foldl' (\acc b -> if b then acc + 1 else acc) 0 pList
  print (show numSet)
  let dispP = toDisplay p
  print (dispP !! 5)
  print (dispP !! 4)
  print (dispP !! 3)
  print (dispP !! 2)
  print (dispP !! 1)
  print (dispP !! 0)
