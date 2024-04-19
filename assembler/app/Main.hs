module Main where

import System.Environment (getArgs, withArgs)
import System.Directory (doesFileExist)

import qualified Parser (extractLabels, purify)
import qualified SymbolTable (createSymbolTable, exists)
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
    in do
        print $ show labels
        print $ show symbols

--translate :: [String] -> [String] -> T.SymbolTable -> [String]
--translate [] binaryLines _ = binaryLines
--translate (assLine:assemblyLines) binaryLines symbols
--    | isVariable = translate assemblyLines binaryLines newSymbols
--    | otherwise = translate assemblyLines newBinaryLines symbols
--    where
--        newSymbols = SymbolTable.add symbols $ Parser.toVariable assLine    -- TODO: implement toVariable which removes @ in front of it and adds it here
--        newBinaryLines = binaryLines ++ [Code.translate assLine]            -- TODO: implement translate
--        isVariable = Parser.isVariable

getLabels :: [String] -> T.SymbolMap
getLabels = Parser.extractLabels

testMain :: [String] -> IO ()
testMain args = withArgs args main
