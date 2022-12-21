module Main where

import System.IO  
import Data.List 
import Data.Foldable
import qualified Data.Text as T
import qualified Data.Sequence as S

type Stacks = [String]
type Stack = String
type Instr = String
type Instrs = [String]
type MoveNum = Int
type FromStack = Int
type ToStack = Int

main :: IO ()
main = do
    withFile "/home/david/code/haskell/adventofcode/2022/day05/input.txt" ReadMode (\handle -> do
        contents <- hGetContents handle 
        let textContents = contents
            ls = lines textContents
            config = take 8 ls
            instrs = drop 10 ls
            stacks = getInitStacks config
        putStrLn $ show $ day5a stacks instrs
        putStrLn $ show $ day5b stacks instrs)



day5a :: Stacks -> Instrs -> String
day5a stacks moveInstrs = 
    let newStacks = processInstrs stacks moveInstrs 1
    in map head newStacks


extractInstr :: String -> (MoveNum, FromStack, ToStack)
extractInstr instr =
    let parts = words instr
    in (stringPartToInt parts 1, stringPartToInt parts 3, stringPartToInt parts 5)

stringPartToInt :: [String] -> Int -> Int
stringPartToInt parts idx = read $ parts !! idx :: Int


processInstrs :: Stacks -> Instrs -> Int -> Stacks
processInstrs origStacks (instr:rest) part = 
    let parts = extractInstr instr
        newStacks = case part of 1 -> processInstr origStacks parts
                                 2 -> processInstr2 origStacks parts
    in  case rest of [] -> newStacks
                     rest -> processInstrs newStacks rest part


processInstr :: Stacks -> (MoveNum, FromStack, ToStack) -> Stacks
processInstr origStacks (0, f, t) = origStacks
processInstr origStacks (x, f, t) = 
    let from = origStacks !! (f-1)
        to   = origStacks !! (t-1)
        newFrom = tail from
        newTo   = ((head from):to) 
        tempOrig = toList $ S.update (f-1) newFrom $ S.fromList origStacks
        newOrig = toList $ S.update (t-1) newTo $ S.fromList tempOrig
    in processInstr newOrig ((x-1), f, t)


processInstr2 :: Stacks -> (MoveNum, FromStack, ToStack) -> Stacks
-- processInstr2 origStacks (0, f, t) = origStacks
processInstr2 origStacks (x, f, t) = 
    let from = origStacks !! (f-1)
        to   = origStacks !! (t-1)
        newFrom = drop x from
        newTo   = (take x from) ++ to
        tempOrig = toList $ S.update (f-1) newFrom $ S.fromList origStacks
        -- newOrig = toList $ S.update (t-1) newTo $ S.fromList tempOrig
    in toList $ S.update (t-1) newTo $ S.fromList tempOrig

getInitStacks :: [String] -> Stacks
getInitStacks ls = map (\z -> T.unpack $ T.stripStart z) (T.transpose (map (\y -> T.concat $ map (\x -> T.drop 1 $ T.take 2 x) (T.chunksOf 4 $ T.pack y)) ls))


day5b :: Stacks -> Instrs -> String
day5b stacks moveInstrs = 
    let newStacks = processInstrs stacks moveInstrs 2
    in map head newStacks

-- Prelude.map (\x -> Data.Text.drop 1 $ Data.Text.take 2 x) (chunksOf 4 $ pack a)


pop :: Stack -> (Char, Stack)
pop (x:xs) = (x, xs)

push :: Char -> Stack -> Stack
push x xs = x:xs

thd :: (a, b, c) -> c
thd (_, _, a) = a
