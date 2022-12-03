module Main where

-- import Data.Text (Text)
-- import qualified Data.Text as T (null, lines, pack)
-- import qualified Data.Text.Read as T (Reader, decimal)
import System.IO  
import System.Directory  
-- import Data.Either (isLeft, rights)

main :: IO ()
main = do
    withFile "/home/david/code/haskell/adventofcode/2022/day02/input.txt" ReadMode (\handle -> do
        contents <- hGetContents handle 
        let textContents = contents
        putStrLn $ show $ day2a textContents
        putStrLn $ show $ day2b textContents)



day2a :: String -> Int
day2a contents = sum $ map getScore $ lines contents

day2b :: String -> Int
day2b contents = sum $ map getScore2 $ lines contents


getScore :: String -> Int
getScore (x:xs) =
  let   tailVal = case (last xs) of 'X' -> 1
                                    'Y' -> 2
                                    'Z' -> 3
        themVal = case x of         'A' -> 1
                                    'B' -> 2
                                    'C' -> 3
        diff = themVal - tailVal
        gscore = case diff of       0 -> 3
                                    -1 -> 6
                                    2 -> 6
                                    _ -> 0
  in tailVal + gscore


getScore2 :: String -> Int
getScore2 (x:xs) =
    let gscore = case (last xs) of  'X' -> 0
                                    'Y' -> 3
                                    'Z' -> 6
                                    _   -> 0
        themVal = case x of         'A' -> 1
                                    'B' -> 2
                                    'C' -> 3
                                    _   -> 0
        objscore = case gscore of   0 -> themVal - 1
                                    3 -> themVal
                                    6 -> themVal + 1
        oscore = case objscore of   0 -> 3
                                    4 -> 1
                                    _ -> objscore
    in gscore + oscore