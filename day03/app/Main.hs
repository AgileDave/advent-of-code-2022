module Main where

import System.IO  
import Data.List 
import Data.Char
import Data.List.Split

main :: IO ()
main = do
    withFile "/home/david/code/haskell/adventofcode/2022/day03/input.txt" ReadMode (\handle -> do
        contents <- hGetContents handle 
        let textContents = contents
        putStrLn $ show $ day3a textContents
        putStrLn $ show $ day3b textContents)


day3a :: String -> Int
day3a contents = sum $ map getPriority $ lines contents


day3b :: String -> Int
day3b contents = sum $ map getBadgePriority $ chunksOf 3 $ lines contents


getPriority :: String -> Int
getPriority line = 
    let sTuple = splitAt ((length line) `div` 2) line
        sharedItem = head $ (fst sTuple) `intersect` (snd sTuple)
        sharedItemOrd = ord sharedItem
    in
        case (isLower sharedItem) of True  -> sharedItemOrd - 96
                                     False -> sharedItemOrd - 38


getBadgePriority :: [String] -> Int
getBadgePriority (x:xs) = 
    let i = head $ (x `intersect` (head xs)) `intersect` (last xs)
        p = ord i
    in case (isLower i) of True  -> p - 96
                           False -> p - 38
