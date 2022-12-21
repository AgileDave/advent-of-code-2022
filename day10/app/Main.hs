module Main where

import System.IO  
import Data.List 
import Text.XML.HXT.DOM.Util as TXU

main :: IO ()
main = do
    withFile "/home/david/code/haskell/adventofcode/2022/day10/input.txt" ReadMode (\handle -> do
        contents <- hGetContents handle 
        let textContents = contents
        putStrLn $ show $ day10a textContents
        putStrLn $ show $ day10b textContents)



day10a :: String -> Int
day10a contents = 
    let cmds = lines contents
        currCmd = head cmds
        restCmds = tail cmds
    in  processCycle currCmd restCmds 1 1 1 0



processCycle :: String -> [String] -> Int -> Int -> Int -> Int -> Int
processCycle currCmd restCmds cycle subcycle xregister subtotal
    | cycle > 220  = subtotal
    | op == "noop" = processCycle (head restCmds) (tail restCmds) (cycle + 1) 1 newXregister newSubtotal
    | op == "addx" = case subcycle of 1 -> processCycle currCmd restCmds (cycle + 1) (getNextSubcycle subcycle) newXregister newSubtotal
                                      2 -> processCycle (head restCmds) (tail restCmds) (cycle + 1) (getNextSubcycle subcycle) newXregister newSubtotal
    where op = getOpCodeValue currCmd 0
          newXregister = case op of "noop" -> xregister
                                    "addx" -> case subcycle of 1 -> xregister
                                                               2 -> xregister  + (convertOpValue $ getOpCodeValue currCmd 1)
          newSubtotal = case ((cycle + 20) `mod` 40) of 0 -> subtotal + (cycle * xregister)
                                                        _ -> subtotal

getOpCodeValue :: String -> Int -> String
getOpCodeValue line idx = words line !! idx

convertOpValue :: String -> Int
convertOpValue val = TXU.decimalStringToInt val

getNextSubcycle :: Int -> Int
getNextSubcycle subcycle = subcycle `mod` 2 + 1

day10b :: String -> Int
day10b contents = 0