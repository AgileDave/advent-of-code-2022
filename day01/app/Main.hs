
module Main where

import System.IO  
import System.Directory  
import Data.Either (isLeft, rights)
import Data.List (maximum, sortOn)
import Data.Ord (Down(Down))
import Data.List.Split (splitWhen)
import Data.Text (Text)
import qualified Data.Text as T (null, lines, pack)
import qualified Data.Text.Read as T (Reader, decimal)

main :: IO ()
main = do
    withFile "/home/david/code/haskell/adventofcode/2022/day01/input.txt" ReadMode (\handle -> do
        contents <- hGetContents handle 
        let textContents = T.pack contents
        putStrLn $ show $ day1a textContents
        putStrLn $ show $ day1b textContents)


parse :: Text -> [Int]
parse = map (sum . rights) . splitWhen isLeft . map (readEntire T.decimal) . T.lines


day1a :: Text -> Int
day1a = maximum . parse

day1b :: Text -> Int
day1b = sum . take 3 . sortOn Down . parse

readEntire :: T.Reader a -> Text -> Either String a
readEntire reader input = do
    (a, t) <- reader input
    if T.null t then Right a else Left "incomplete read"