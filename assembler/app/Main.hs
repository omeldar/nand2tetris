module Main where

import System.Environment (getArgs, withArgs)
import System.Directory (doesFileExist)

import qualified Parser (extractLabels, purify)
import qualified Code
import qualified Types as T

processFile :: String -> IO ()
processFile filename = do
    fileExists <- doesFileExist filename
    if fileExists
        then do
            content <- Parser.purify $ readFile filename
            assemble content
        else putStrLn $ "File " ++ filename ++ " does not exist."

handleArgs :: [String] -> IO ()
handleArgs args = case args of
    [file] -> processFile file
    [] -> processFile "Prog.asm"
    _ -> putStrLn "Please provide zero or one argument."

main :: IO ()
main = getArgs >>= handleArgs

assemble :: String -> IO ()
assemble content = do
    print $ getLabels content

getLabels :: String -> T.SymbolTable
getLabels content = Parser.extractLabels $ lines content

-- helper method used for testing and working with ghci
testMain :: [String] -> IO ()
testMain args = withArgs args main
