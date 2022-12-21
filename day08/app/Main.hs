module Main where

import System.IO  
import Data.List 

main :: IO ()
main = do
    withFile "/home/david/code/haskell/adventofcode/2022/day08/input.txt" ReadMode (\handle -> do
        contents <- hGetContents handle 
        let textContents = contents
        putStrLn $ show $ day8a textContents
        putStrLn $ show $ day8b textContents)



day8a :: String -> Int
day8a line = 0


day8b :: String -> Int
day8b line = 0