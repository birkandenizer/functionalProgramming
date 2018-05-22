import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import System.Environment
import System.IO
import Prelude hiding (Word)

data Trie = Trie {end :: Bool, children :: Map Char Trie} deriving (Show)
type Word = String

--Creates an empty trie
empty :: Trie
empty = Trie {end = False, children = Map.empty}

--Inserts a given word to a given trie
insert :: Word -> Trie -> Trie
insert [] tree = tree { end = True }
insert (x:xs) tree = let treeChild = children tree
    in case Map.lookup x treeChild of
        Nothing -> tree {children = Map.insert x (insert xs empty) treeChild}
        Just tree' -> tree {children = Map.insert x (insert xs tree') treeChild}

--Takes list of words and returns them as a trie
insertList :: [Word] -> Trie
insertList = foldr insert empty

--Checks whether a given word exists in a given Trie
search :: Word -> Trie -> Bool
search [] (Trie e _) = e
search (x:xs) (Trie _ child) = fromMaybe False (fmap (search xs) (Map.lookup x child))

getWords :: Trie -> [Word]
getWords = undefined

prefix :: Word -> Trie -> Maybe [Word]
prefix = undefined

--Takes a Trie, reads a word from user, adds that word to the Trie
addWord :: Trie -> IO Trie
addWord tree = do
    putStrLn "Enter word/prefix:"
    line <- getLine
    let newTree = insert line tree
    --Uncomment next line to see that word was successfully inserted into the Trie
    --print newTree
    putStrLn "New word is added!"
    return newTree

--Takes a Trie, reads a word from user, searches that word in the Trie
searchWord :: Trie -> IO ()
searchWord tree = do
    putStrLn "Enter word/prefix:"
    line <- getLine
    let outcome = search line tree
    if outcome == True 
        then putStrLn "Exist in dictionary!"
        else putStrLn "NOT exist!"

findWord :: Trie -> IO ()
findWord = undefined

printAllWords :: Trie -> IO ()
printAllWords = undefined

--Helper function to get action from user
printMenu :: Trie -> IO ()
printMenu tree = do
    putStrLn ""
    putStrLn "a) Add Word"
    putStrLn "s) Search Word"
    putStrLn "f) Find words with prefix"
    putStrLn "p) Print all words"
    putStrLn "e) Exit"
    putStrLn "Enter the action:"
    [character] <- getLine

    case character of
        'a' -> do {tree' <- addWord tree; printMenu tree'}
        's' -> do {searchWord tree; printMenu tree}
        'f' -> do {findWord tree; printMenu tree}
        'p' -> do {printAllWords tree; printMenu tree}
        'e' -> return ()
        _ -> do {putStrLn "Invalid action!"; printMenu tree}

main :: IO ()
main = do
    [arg] <- getArgs
    content <- readFile arg
    let separatedLines = lines content
    let a = insertList separatedLines
    --Uncomment next line to see that words.txt was successfully inserted into a Trie
    --print a
    printMenu a