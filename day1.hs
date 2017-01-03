--  Haskell code to solve the problem presented in Advent of Code 2016, day 1.
--  Author: Bob Wall
module AOCDay1 where

import Data.List
import Data.Set

data Direction = North | East | South | West deriving (Enum, Show) 

turn :: Direction -> Char -> Direction
turn curDir rotate =
  toEnum (mod nextDirNum 4) :: Direction
  where
    dirNum = fromEnum curDir
    nextDirNum = if (rotate == 'R') then dirNum + 1 else dirNum - 1 

type Move = (Char, Int)
type Point = (Int, Int)
type Position = (Direction, Point)
type Path = Set (Int, Int)
type Track = [Point]
type PositionWithPath = (Position, Path)


getNextPoint :: Direction -> Int -> Point -> Point
getNextPoint North dist (x, y) = (x,        y + dist)
getNextPoint East  dist (x, y) = (x + dist, y)
getNextPoint South dist (x, y) = (x,        y - dist)
getNextPoint West  dist (x, y) = (x - dist, y)


parseInput :: String -> [Move]
parseInput inp = 
  Data.List.map (\s -> ((head s), (read (tail s)))) strs
  where
    strs = Data.List.map (Data.List.filter (/= ',')) (words inp)


solution1p1 :: [Move] -> Int
solution1p1 moves =
  (abs (fst finalPoint)) + (abs (snd finalPoint))
  where
    startPosition = (North, (0, 0))
    nextPosition :: Position -> Move -> Position
    nextPosition curPosition nextMove =
      (nextDir, nextPoint)
      where
        nextDir = turn (fst curPosition) (fst nextMove)
        nextPoint = getNextPoint nextDir (snd nextMove) (snd curPosition)
    finalPoint = snd (Data.List.foldl' nextPosition startPosition moves)


getTrack :: Direction -> Int -> Point -> Track
getTrack North dist (x, y) = reverse(Data.List.foldl' (\acc y -> (x, y) : acc) [] [y .. y + dist])
getTrack East  dist (x, y) = reverse(Data.List.foldl' (\acc x -> (x, y) : acc) [] [x .. x + dist])
getTrack South dist (x, y) = reverse(Data.List.foldl' (\acc y -> (x, y) : acc) [] [y, y - 1 .. y - dist])
getTrack West  dist (x, y) = reverse(Data.List.foldl' (\acc x -> (x, y) : acc) [] [x, x - 1 .. x - dist])



solution1p2 :: [Move] -> Int
solution1p2 moves = do
  (abs (fst intersectPoint)) + (abs (snd intersectPoint))
  where
    startPositionWithPath = ((North, (0, 0)), empty)
    moveUntilIntersect :: [Move] -> PositionWithPath -> PositionWithPath
    moveUntilIntersect [] curPositionWithPath = curPositionWithPath
    moveUntilIntersect (nextMove : moves) curPositionWithPath =
      case foundIntersect of
        Just (x, y) -> (((fst curPosition), (x, y)), curPath)
        Nothing -> moveUntilIntersect moves ((nextDirection, last track), (Data.Set.union curPath (fromList (init track))))
      where
        curPosition = fst curPositionWithPath
        nextDirection = turn (fst curPosition) (fst nextMove)
        curPath = snd curPositionWithPath
        track = getTrack nextDirection (snd nextMove) (snd curPosition)
        foundIntersect = Data.List.foldr (\point remainder -> if (member point curPath) then Just point else remainder) Nothing track
    intersectPoint = snd (fst (moveUntilIntersect moves startPositionWithPath))


printDay1Solutions :: IO ()
printDay1Solutions = do
  content <- readFile "day1.in"
  let moves = parseInput content
  print (show (solution1p1 moves))
  print (show (solution1p2 moves))
