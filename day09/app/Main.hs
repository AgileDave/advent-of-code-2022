module Main where

import System.IO  
import Data.List 

main :: IO ()
main = do
    withFile "/home/david/code/haskell/adventofcode/2022/day09/input.txt" ReadMode (\handle -> do
        contents <- hGetContents handle 
        let textContents = contents
        putStrLn $ show $ day9a textContents
        putStrLn $ show $ day9b textContents)



day9a :: String -> Int
day9a line = 0


day9b :: String -> Int
day9b line = 0