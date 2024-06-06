module Main where

import System.Environment (getArgs, withArgs)
import System.Directory (doesFileExist)

import qualified Parser (extractLabels, purify)
import qualified SymbolTable (createSymbolTable)
import qualified Types as T
import qualified Code (translate)

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
        binary = Code.translate symbols labels content []
    in do
        print $ "labels: " ++ show labels
        print $ "symbols: " ++ show symbols
        print $ "result: " ++ show binary

getLabels :: [String] -> T.SymbolTable
getLabels = Parser.extractLabels

testMain :: [String] -> IO ()
testMain args = withArgs args main
