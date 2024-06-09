module Main where

import System.Environment (getArgs, withArgs)
import System.Directory (doesFileExist)

import qualified Parser (extractLabels, purify)
import qualified SymbolTable (createSymbolTable)
import qualified Code (translate)

processFile :: String -> IO ()
processFile filename = do
    fileExists <- doesFileExist filename
    if fileExists
        then do
            contents <- readFile filename
            let purifiedLines = Parser.purify $ lines contents
            assemble purifiedLines filename
        else
            putStrLn $ "File " ++ filename ++ " does not exist."

handleArgs :: [String] -> IO ()
handleArgs args = case args of
    [file] -> processFile file
    [] -> processFile "Prog.asm"
    _ -> putStrLn "Please provide zero or one argument."

main :: IO ()
main = getArgs >>= handleArgs

assemble :: [String] -> String -> IO ()
assemble content filename =
    let labels = Parser.extractLabels content
        symbols = SymbolTable.createSymbolTable
        binary = Code.translate symbols labels content [] 16
    in do
        writeFile (createOutPath filename) (unlines binary)
        print $ "Binary output written to " ++ createOutPath filename

createOutPath :: String -> String
createOutPath filename = baseName ++ ".hack"
  where
    baseName = reverse . drop 1 . dropWhile (/= '.') . reverse $ filename

-- used when testing within ghci
testMain :: [String] -> IO ()
testMain args = withArgs args main
