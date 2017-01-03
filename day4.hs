--  Haskell code to solve the problem presented in Advent of Code 2016, day 4.
--  Author: Bob Wall
module AOCDay4 where

import Data.Char
import Data.List


--  Room names are formatted as a string containing lower case letters (possibly interspersed with dash), a
--  dash, a sector number, a dash, and a checksum enclosed in "[]". For example, "b-abcabc-def-123[bacde]"
--  A valid checksum is a list of the five most commonly occurring characters, in descending order of frequency,
--  with tied frequencies sorted alphabetically.
getChecksum :: String -> String
getChecksum str = takeWhile (/= ']') (tail (dropWhile (/= '[') str))

getSectorID :: String -> Int
getSectorID str = read (filter isDigit str)

getRoomName :: String -> String
getRoomName str = init (takeWhile (\c -> not (isDigit c)) str)

countChar :: String -> Char -> Int
countChar str c = length (filter (== c) str)

getFrequencies :: String -> [(Char, Int)]
getFrequencies str =
  take 5 sortedList
  where
    freqs = [countChar str c | c <- ['a'..'z']]
    flist = zip ['a'..'z'] freqs
    sortedList = sortBy 
                   (\a b -> if ((snd a) == (snd b)) then compare (fst a) (fst b) else compare (snd b) (snd a))
                   flist

computeChecksum :: String -> String
computeChecksum str = foldr (\pair acc -> (fst pair) : acc) [] (getFrequencies (getRoomName str))


solutions4p1 :: [String] -> Int
solutions4p1 rooms =
  foldl' (\acc room -> acc + (getSectorID room)) 0 rooms

rotateChar :: Int -> Char -> Char
rotateChar _ '-' = ' '
rotateChar i c = chr((ord 'a') + (mod (((ord c) - (ord 'a')) + i ) 26))

decryptRoom :: String -> (String, Int)
decryptRoom room =
  (map (rotateChar sectorID) roomName, sectorID)
  where
    roomName = getRoomName room
    sectorID = getSectorID room
    

printDay4Solutions :: IO ()
printDay4Solutions = do
  content <- readFile "day4.in"
  let rooms = lines content
  let validRooms = filter (\room -> (computeChecksum room) == (getChecksum room)) rooms
  print (show (solutions4p1 validRooms))
  let decryptedRooms = map decryptRoom validRooms
  print (show (filter (\room -> isInfixOf "north" (fst room)) decryptedRooms))
