module Main where

import System.Environment (getArgs, withArgs)
import System.Directory (doesFileExist)

import qualified Parser (extractLabels, purify)
import qualified SymbolTable (createSymbolTable)
import qualified Types as T

processFile :: String -> IO ()
processFile filename = do
    fileExists <- doesFileExist filename
    if fileExists
        then do
            contents <- readFile filename
            let purifiedLines = Parser.purify $ lines contents
            assemble purifiedLines
        else
            putStrLn $ "File " ++ filename ++ " does not exist."

handleArgs :: [String] -> IO ()
handleArgs args = case args of
    [file] -> processFile file
    [] -> processFile "Prog.asm"
    _ -> putStrLn "Please provide zero or one argument."

main :: IO ()
main = getArgs >>= handleArgs

assemble :: [String] -> IO ()
assemble content =
    let labels = getLabels content
        symbols = SymbolTable.createSymbolTable
    in do
        print $ show labels
        print $ show symbols

getLabels :: [String] -> T.SymbolMap
getLabels = Parser.extractLabels

testMain :: [String] -> IO ()
testMain args = withArgs args main
