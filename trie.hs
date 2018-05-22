import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import System.Environment
import System.IO
import Prelude hiding (Word)

data Trie = Trie {end :: Bool, children :: Map Char Trie} deriving (Show)
type Word = String

empty :: Trie
empty = Trie {end = False, children = Map.empty}

insert :: Word -> Trie -> Trie
insert [] tree = tree { end = True }
insert (x:xs) tree = tree {children = Map.alter ( \l -> Just(insert xs (fromMaybe empty l))) x $ children tree}

insertList :: [Word] -> Trie
insertList = foldr insert empty

search :: Word -> Trie -> Bool
search [] (Trie e _) = e
search (x:xs) (Trie _ child) = fromMaybe False (fmap (search xs) (Map.lookup x child))

getWords :: Trie -> [Word]
getWords = undefined

prefix :: Word -> Trie -> Maybe [Word]
prefix = undefined

addWord :: IO String
addWord = do
    getWord

searchWord :: IO String
searchWord = do
    getWord

findWord :: IO String
findWord = do
    getWord

printAllWords :: IO String
printAllWords = do
    getWord

printMenu :: IO String
printMenu = do
    putStrLn "a) Add Word"
    putStrLn "s) Search Word"
    putStrLn "f) Find words with prefix"
    putStrLn "p) Print all words"
    putStrLn "e) Exit"
    putStrLn "Enter the action:"
    character <- getChar

    case character of
        'a' -> addWord
        's' -> searchWord
        'f' -> findWord
        'p' -> printAllWords 
        'e' -> return "Exit"
        _ -> return "Wrong action, please try again!"

getWord :: IO String
getWord = do
    putStrLn "Enter word/prefix:"
    line <- getLine
    return line

main :: IO String
main = do
    [arg] <- getArgs
    content <- readFile arg
    let separatedLines = lines content
    let a = insertList separatedLines
    printMenu