module Main where

import System.IO  
import Data.List 
import Data.List.Split

main :: IO ()
main = do
    withFile "/home/david/code/haskell/adventofcode/2022/day04/input.txt" ReadMode (\handle -> do
        contents <- hGetContents handle 
        let textContents = contents
        putStrLn $ show $ day4a textContents
        putStrLn $ show $ day4b textContents)



day4a :: String -> Int
day4a contents = length $ filter (==True) $ map checkFullyContain $ lines contents


day4b :: String -> Int
day4b contents = length $ filter (==True) $ map checkOverlap $ lines contents


checkFullyContain :: String -> Bool
checkFullyContain line = 
    let (a,b,c,d) = getIntParts line
    in  ((a,b) `containedBy` (c,d)) || ((c,d) `containedBy` (a,b))


checkOverlap :: String -> Bool
checkOverlap line =
    let (a,b,c,d) = getIntParts line
    in ((a `between` (c,d)) || (b `between` (c,d))) || ((c `between` (a,b)) || (d `between` (a,b)))


getIntParts :: String -> (Integer, Integer, Integer, Integer)
getIntParts line = 
    let parts = splitOneOf "-," line
        intParts = map (\z -> read z :: Integer) parts
        a = intParts !! 0
        b = intParts !! 1
        c = intParts !! 2
        d = intParts !! 3
    in (a,b,c,d)

between :: Integer -> (Integer, Integer) -> Bool
between point (start,end) = point >= start && point <= end

containedBy :: (Integer, Integer) -> (Integer, Integer) -> Bool
containedBy (innerStart,innerEnd) (outerStart,outerEnd) = (innerStart >= outerStart) && (innerEnd <= outerEnd)