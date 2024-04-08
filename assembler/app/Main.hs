module Main where

import System.IO
import System.Environment (getArgs, withArgs)
import System.Directory (doesFileExist)

processFile :: String -> IO ()
processFile filename = do
    fileExists <- doesFileExist filename
    if fileExists
        then do
            content <- readFile filename
            putStrLn "File content:"
            putStrLn content
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
