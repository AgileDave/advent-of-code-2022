module Main where

import System.IO  
import Data.List 

main :: IO ()
main = do
    withFile "/home/david/code/haskell/adventofcode/2022/day04/input.txt" ReadMode (\handle -> do
        contents <- hGetContents handle 
        let textContents = contents
        putStrLn $ show $ day4a textContents
        putStrLn $ show $ day4b textContents)



day4a :: String -> Int
day4a contents = 0


day4b :: String -> Int
day4b contents = 0