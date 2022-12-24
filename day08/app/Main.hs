module Main where

import System.IO  
import Data.List 

main :: IO ()
main = do
    withFile "/home/david/code/haskell/adventofcode/2022/day08/input.txt" ReadMode (\handle -> do
        contents <- hGetContents handle 
        let textContents = contents
            rows = lines textContents
            cols = transpose rows
        putStrLn $ show $ day8a rows cols
        putStrLn $ show $ day8b rows cols)



day8a :: [String] -> [String] -> Int
day8a rows cols = 
    let maxCol = (length cols) - 1
        maxRow = (length rows) - 1
    in  day8a' rows cols 0 0 maxRow maxCol 0


day8a' :: [String] -> [String] -> Int -> Int -> Int -> Int -> Int -> Int
day8a' rows cols currRow currCol maxRow maxCol currCount 
    | currCol == maxCol && currRow == maxRow = (currCount + 1)
    | currRow == 0 && currCol == maxCol = day8a' rows cols 1 0 maxRow maxCol (currCount + 1)
    | currRow == 0 = day8a' rows cols 0 (currCol + 1) maxRow maxCol (currCount + 1)
    | currCol == 0 = day8a' rows cols currRow (currCol + 1) maxRow maxCol (currCount + 1)
    | currCol == maxCol = day8a' rows cols (currRow + 1) 0 maxRow maxCol (currCount + 1)
    | currRow == maxRow = day8a' rows cols currRow (currCol + 1) maxRow maxCol (currCount + 1)
    | otherwise = 
        let row = rows !! currRow
            col = cols !! currCol
            newCount = case (canBeSeen row col currRow currCol) of True -> currCount + 1
                                                                   _    -> currCount
        in day8a' rows cols currRow (currCol + 1) maxRow maxCol newCount


canBeSeen :: String -> String -> Int -> Int -> Bool
canBeSeen row col currRow currCol =
    let height = row !! currCol
        isSeen s p h = (foldr (\x acc -> acc && x < h) True $ take p s) || (foldr (\x acc -> acc && x < h) True $ drop (p + 1) s)
    in  case (isSeen row currCol height) of True -> True
                                            False -> isSeen col currRow height

day8b :: [String] -> [String] -> Int
day8b rows cols =
    let maxCol = (length cols) - 1
        maxRow = (length rows) - 1
    in  day8b' rows cols 1 1 maxRow maxCol 0
    

day8b' :: [String] -> [String] -> Int -> Int -> Int -> Int -> Int -> Int
day8b' rows cols currRow currCol maxRow maxCol maxVisibility 
    | currRow == maxRow = maxVisibility 
    | currCol == 0 = day8b' rows cols currRow 1 maxRow maxCol maxVisibility
    | currCol == maxCol = day8b' rows cols (currRow + 1) 1 maxRow maxCol maxVisibility
    | otherwise = 
        let row = rows !! currRow
            col = cols !! currCol
            rowParts = splitAt currCol row
            colParts = splitAt currRow col
            item = row !! currCol
            leftPart = fst rowParts
            rightPart = drop 1 $ snd rowParts
            upPart = fst colParts
            downPart = drop 1 $ snd colParts
            left = setMinVal' leftPart $ length $ takeWhile (< item) $ reverse $ leftPart
            right = setMinVal' rightPart $ length $ takeWhile (< item) $ rightPart
            up = setMinVal' upPart $ length $ takeWhile (< item) $ reverse $ upPart
            down = setMinVal' downPart $ length $ takeWhile (< item) $ downPart
            tempMaxVis = left * right * up * down
            newMaxVis = if (tempMaxVis > maxVisibility) then tempMaxVis else maxVisibility
        in day8b' rows cols currRow (currCol + 1) maxRow maxCol newMaxVis

setMinVal :: Int -> Int
setMinVal val = if val == 0 then 1 else val

setMinVal' :: String -> Int -> Int
setMinVal' part calcLen = if (==) calcLen $ length part
                            then calcLen
                            else calcLen + 1