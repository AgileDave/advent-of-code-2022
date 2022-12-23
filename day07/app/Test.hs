import Data.Tree

-- Define a type for file names
type FileName = String

-- Define a type for file contents
type FileContents = String
type Size = Int

-- Define a type for a file system tree
data FileSystemTree = File FileName Size
                   | Directory FileName [FileSystemTree]
                   deriving (Show)

-- Convert a FileSystemTree to a Tree
toTree :: FileSystemTree -> Tree String
toTree (File name size) = Node (name,size) []
toTree (Directory name children) = Node name (map toTree children)

-- Create a sample file system tree
sampleTree :: FileSystemTree
sampleTree = Directory "root"
             [ File "file1.txt" 1000
             , Directory "dir1"
               [ File "file2.txt" 2000
               , File "file3.txt" 3000
               ]
             , File "file4.txt" 4000
             ]

-- Convert the sample file system tree to a Tree and print it
main :: IO ()
main = putStrLn (drawTree (toTree sampleTree))
