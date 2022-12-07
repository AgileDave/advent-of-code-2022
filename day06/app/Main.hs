module Main where

import System.IO  
import Data.List 

main :: IO ()
main = do
    withFile "/home/david/code/haskell/adventofcode/2022/day06/input.txt" ReadMode (\handle -> do
        contents <- hGetContents handle 
        let textContents = contents
        putStrLn $ show $ day6a textContents
        putStrLn $ show $ day6b textContents)



day6a :: String -> Int
day6a line = 
    let (marker,rest) = splitAt 4 line
        lengthOfUniqueCharsInMarker = length $ nub marker
    in case lengthOfUniqueCharsInMarker of 4 -> 4
                                           _ -> findUniqueMarkerPoint ((tail marker) ++ [(head rest)]) (tail rest) 5 4

findUniqueMarkerPoint :: String -> String -> Int -> Int -> Int
findUniqueMarkerPoint marker rest currPos uniqueLength = 
    let lengthOfUniqueCharsInMarker = length $ nub marker
    in case (lengthOfUniqueCharsInMarker == uniqueLength) of True -> currPos
                                                             False -> findUniqueMarkerPoint ((tail marker) ++ [(head rest)]) (tail rest) (currPos + 1) uniqueLength

day6b :: String -> Int
day6b line = 
    let (marker,rest) = splitAt 14 line
        lengthOfUniqueCharsInMarker = length $ nub marker
    in case lengthOfUniqueCharsInMarker of 14 -> 14
                                           _ -> findUniqueMarkerPoint ((tail marker) ++ [(head rest)]) (tail rest) 15 14