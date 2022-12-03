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
        putStrLn $ show $ day2a textContents
        putStrLn $ show $ day2b textContents)


day2a :: String -> Int
day2a contents = sum $ map getPriority $ lines contents


day2b :: String -> Int
day2b contents = sum $ map getBadgePriority $ chunksOf 3 $ lines contents


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
