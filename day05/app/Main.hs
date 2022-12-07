module Main where

import System.IO  
import Data.List 
import Data.Stack

main :: IO ()
main = do
    withFile "/home/david/code/haskell/adventofcode/2022/day05/input.txt" ReadMode (\handle -> do
        contents <- hGetContents handle 
        let textContents = contents
        putStrLn $ show $ day5a textContents
        putStrLn $ show $ day5b textContents)



day5a :: String -> Int
day5a line = 0


day5b :: String -> Int
day5b line = 0

-- Prelude.map (\x -> Data.Text.drop 1 $ Data.Text.take 2 x) (chunksOf 4 $ pack a)