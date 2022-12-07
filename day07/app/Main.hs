module Main where

import System.IO  
import Data.List 

main :: IO ()
main = do
    withFile "/home/david/code/haskell/adventofcode/2022/day07/input.txt" ReadMode (\handle -> do
        contents <- hGetContents handle 
        let textContents = contents
        putStrLn $ show $ day7a textContents
        putStrLn $ show $ day7b textContents)



day7a :: String -> Int
day7a line = 0


day7b :: String -> Int
day7b line = 0