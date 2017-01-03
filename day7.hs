--  Haskell code to solve the problem presented in Advent of Code 2016, day 7.
--  Author: Bob Wall
module AOCDay7 where

import Data.Char
import Data.List


hasAbba :: String -> Bool
hasAbba s =
  if (length s < 4) then False
  else
    if ((head s) == (s !! 3) && (head s) /= (s !! 1) && (s !! 1) == (s !! 2)) then True
    else hasAbba (tail s)

hasAnyAbba :: [String] -> Bool
hasAnyAbba parts =
  foldr (\s acc -> if (hasAbba s) then True else acc) False parts


notDelim :: Char -> Bool
notDelim c = (c /= '[') && (c /= ']')

breakAddr :: String -> [String]
breakAddr addr =
  subBreak [] addr
  where
    subBreak :: [String] -> String -> [String]
    subBreak parts [] = reverse parts
    subBreak parts addr =
      subBreak (nextWord : parts) remaining
      where
        nextWord = takeWhile notDelim addr
        after = drop (length nextWord) addr
        remaining = if (length after > 0) then tail after else after

isTLSAddr :: String -> Bool
isTLSAddr addr =
  (hasAnyAbba nonhypernets) && (not (hasAnyAbba hypernets))
  where
    addrParts = zip [0..] (breakAddr addr)
    hypernets = map snd (filter (\x -> odd (fst x)) addrParts)
    nonhypernets = map snd (filter (\x -> even (fst x)) addrParts)
  
getAbas :: String -> [String]
getAbas addr =
   subAba addr []
   where
     subAba :: String -> [String] -> [String]
     subAba [] abas = abas
     subAba addr abas =
       if (length addr < 3) then abas
       else
         if ((head addr) == (addr !! 2) && (head addr) /= (addr !! 1)) then
           subAba (tail addr) ((take 3 addr) : abas)
         else subAba (tail addr) abas

invertAba :: String -> String
invertAba aba = [(aba !! 1), (head aba), (aba !! 1)]

isSSLAddr :: String -> Bool
isSSLAddr addr =
  foldr (\bab acc -> if (foldr (\hn acc -> if (isInfixOf bab hn) then True else acc) False hypernets) then True else acc) False babs
  where
    addrParts = zip [0..] (breakAddr addr)
    hypernets = map snd (filter (\x -> odd (fst x)) addrParts)
    nonhypernets = map snd (filter (\x -> even (fst x)) addrParts)
    abas = concat (map getAbas nonhypernets)
    babs = map invertAba abas
  

printDay7Solutions :: IO ()
printDay7Solutions = do
  content <- readFile "day7.in"
  let addrs = lines content
  print (show (length (filter isTLSAddr addrs)))
  print (show (length (filter isSSLAddr addrs)))
