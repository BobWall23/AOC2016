--  Haskell code to solve the problem presented in Advent of Code 2016, day 5.
--  Author: Bob Wall
module AOCDay5 where

import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Lazy  as LB
import Data.Digest.Pure.MD5
import Data.List
import Data.Char


doorID :: String
doorID = "ffykfhsq"

compHash :: Int -> String
compHash i = show (md5 (LB.fromStrict (BSC8.pack (doorID ++ (show i)))))

hashes = [compHash i | i <- [1..]]
goodHashes = filter (\h -> isPrefixOf "00000" h) hashes

firstEight = take 8 goodHashes

--password = concat (map (\str -> take 1 (drop 5 str)) firstEight)
password = map (flip (!!) 5) firstEight


buildList :: [(Int, Char)] -> [String] -> [(Int, Char)]
buildList acc (hash : hashes) =
  if (position >= 0 && position <= 7 && length (filter (\x -> (fst x) == position) acc) == 0)
    then do
      let newAcc = (position, char) : acc
      if ((length newAcc) == 8) then newAcc else (buildList newAcc hashes)
    else buildList acc hashes
  where
    position = ord(hash !! 5) - ord('0')
    char = (hash !! 6)
  
newList = buildList [] goodHashes
newPassword = map snd (sort newList)
