module Main where

import System.Environment (getArgs, withArgs)
import System.Directory (doesFileExist)

import qualified Parser (parse)
import qualified Code
import qualified SymbolTable

processFile :: String -> IO ()
processFile filename = do
    fileExists <- doesFileExist filename
    if fileExists
        then do
            content <- readFile filename
            putStrLn $ Parser.parse content
        else putStrLn $ "File " ++ filename ++ " does not exist."

handleArgs :: [String] -> IO ()
handleArgs args = case args of
    [file] -> processFile file
    [] -> processFile "Prog.asm"
    _ -> putStrLn "Please provide zero or one argument."

main :: IO ()
main = getArgs >>= handleArgs

-- helper method used for testing and working with ghci
testMain :: [String] -> IO ()
testMain args = withArgs args main
