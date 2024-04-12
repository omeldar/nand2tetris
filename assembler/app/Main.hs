module Main where

import System.Environment (getArgs, withArgs)
import System.Directory (doesFileExist)

import qualified Parser (extractLabels, purify)
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
    in print $ show labels

getLabels :: [String] -> T.SymbolMap
getLabels content = Parser.extractLabels content

testMain :: [String] -> IO ()
testMain args = withArgs args main
