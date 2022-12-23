module Main where

import System.IO 
import qualified Data.Map as M
import Data.List 

type CurrentPath = [String]
type DirSize = Int
type FileSize = Int
type Directories = M.Map CurrentPath DirSize
type CommandList = [String]

main :: IO ()
main = do
    withFile "/home/david/code/haskell/adventofcode/2022/day07/input.txt" ReadMode (\handle -> do
        contents <- hGetContents handle 
        let textContents = contents
            cmds = lines textContents
        putStrLn $ show $ day7a cmds
        putStrLn $ show $ day7b cmds)



day7a :: CommandList -> Int
day7a cmds = 
    let dirs = createDirectoryListing [] M.empty cmds
        f a acc = acc + a
    in M.foldr f 0 (M.filter (< 100001) dirs)

createDirectoryListing :: CurrentPath -> Directories -> CommandList -> Directories
createDirectoryListing path dirs (cmd:cmds)
    | cmd == "$ cd /" = 
        let newPath = ["/"]
            newDirs = M.insert newPath 0 dirs
        in createDirectoryListing newPath newDirs cmds
    | cmd == "$ ls" = createDirectoryListing path dirs cmds
    | cmd == "$ cd .." = createDirectoryListing (init path) dirs cmds
    | "$ cd " `isPrefixOf` cmd = 
        let newDir = words cmd !! 2
            newPath = path ++ [newDir]
        in createDirectoryListing newPath dirs cmds
    | "dir " `isPrefixOf` cmd = 
        let newDir = words cmd !! 1
            tempPath = path ++ [newDir]
            newDirs = case (M.member tempPath dirs) of False -> M.insert tempPath 0 dirs
                                                       True  -> dirs
        in createDirectoryListing path newDirs cmds
    | otherwise = 
        let fileSize = read (words cmd !! 0) :: Int
            newDirs = updateParentDirs path dirs fileSize
        in case (null cmds) of True -> newDirs
                               False -> createDirectoryListing path newDirs cmds


updateParentDirs :: CurrentPath -> Directories -> FileSize -> Directories
updateParentDirs path dirs fileSize 
    | null path = dirs
    | otherwise = 
        let newDirs = M.adjust ((+) fileSize) path dirs
        in updateParentDirs (init path) newDirs fileSize

day7b :: CommandList -> Int
day7b cmds = 
    let dirs = createDirectoryListing [] M.empty cmds
        maxSpace = 70000000
        neededFreeSpace = 30000000
        usedSpace = dirs M.! ["/"]
        unusedSpace = maxSpace - usedSpace
        requiredSpace = neededFreeSpace - unusedSpace
        f a acc = min a acc
    in minimum $ M.elems $ (M.filter (>= requiredSpace) dirs)