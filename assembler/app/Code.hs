module Code (
    translate
) where

import Data.Char (isDigit)

import qualified Types as T
import qualified Data.Map as M

import Debug.Trace (trace) -- delete later

translate :: T.SymbolTable -> T.LabelMap -> [String] -> [String] -> Int -> [String]
translate _ _ [] binary _ = reverse binary
translate symbols labels (line:rest) binary nextAddress =
    let (newBinary, newSymbols, newNextAddress) = translateLine symbols labels line binary nextAddress
    in translate newSymbols labels rest newBinary newNextAddress

translateLine :: T.SymbolTable -> T.LabelMap -> String -> [String] -> Int -> ([String], T.SymbolTable, Int)
translateLine symbols labels line binary nextAddress
    | isAInstruction line = 
        let (translated, newSymbols, newNextAddress) = translateAInstruction symbols labels line nextAddress
        in (binary ++ [translated], newSymbols, newNextAddress)
    | isCInstruction line = 
        let translated = translateCInstruction line
        in (binary ++ [translated], symbols, nextAddress)
    | otherwise = error $ "Invalid instruction found: '" ++ line ++ "'"

isAInstruction :: String -> Bool
isAInstruction ('@':_) = True
isAInstruction _ = False

isCInstruction :: String -> Bool
isCInstruction line = not (null line) && head line /= '@'

translateAInstruction :: T.SymbolTable -> T.LabelMap -> String -> Int -> (String, T.SymbolTable, Int)
translateAInstruction symbols labels line nextAddress = case parseAInstruction symbols labels line of
    Left address -> (intToBinary address, symbols, nextAddress)
    Right newSymbol ->
        let updatedSymbols = M.insert newSymbol nextAddress symbols
        in (intToBinary nextAddress, updatedSymbols, nextAddress + 1)

parseAInstruction :: T.SymbolTable -> T.LabelMap -> String -> Either Int T.Symbol
parseAInstruction symbols labels ('@':rest)
    | all isDigit rest = Left (read rest)
    | otherwise = case M.lookup rest labels of
        Just address -> Left address
        Nothing -> case M.lookup rest symbols of
            Just address -> Left address
            Nothing -> Right rest
parseAInstruction _ _ _ = error "Invalid A-instruction format"

translateCInstruction :: String -> String
translateCInstruction line = "111" ++ compBits ++ destBits ++ jumpBits
    where
        (comp, dest, jump) = parseCInstruction line
        compBits = compMap M.! comp
        destBits = destMap M.! dest
        jumpBits = jumpMap M.! jump

parseCInstruction :: String -> (String, String, String)
parseCInstruction line = (comp, dest', jump')
    where
        (destComp, jump) = break (== ';') line
        (dest, comp') = break (== '=') destComp
        comp = if null comp' then destComp else tail comp' -- to remove the '=' if present
        dest' = if null comp' then "null" else dest -- set dest to "null" if '=' is missing
        jump' = if null jump then "null" else tail jump -- to remove the ';' if present

intToBinary :: Int -> String
intToBinary x = replicate (16 - length bin) '0' ++ bin
    where
        bin = toBinary x

toBinary :: Int -> String
toBinary 0 = "0"
toBinary n
    | n > 0 = toBinary (n `div` 2) ++ show (n `mod` 2)
    | otherwise = error "Negative numbers cannot be converted to binary"

compMap :: M.Map String String
compMap = M.fromList [
    ("0",   "0101010"),
    ("1",   "0111111"),
    ("-1",  "0111010"),
    ("D",   "0001100"),
    ("A",   "0110000"),
    ("M",   "1110000"),
    ("!D",  "0001101"),
    ("!A",  "0110001"),
    ("!M",  "1110001"),
    ("-D",  "0001111"),
    ("-A",  "0110011"),
    ("-M",  "1110011"),
    ("D+1", "0011111"),
    ("A+1", "0110111"),
    ("M+1", "1110111"),
    ("D-1", "0001110"),
    ("A-1", "0110010"),
    ("M-1", "1110010"),
    ("D+A", "0000010"),
    ("D+M", "1000010"),
    ("D-A", "0010011"),
    ("D-M", "1010011"),
    ("A-D", "0000111"),
    ("M-D", "1000111"),
    ("D&A", "0000000"),
    ("D&M", "1000000"),
    ("D|A", "0010101"),
    ("D|M", "1010101")
    ]

destMap :: M.Map String String
destMap = M.fromList [
    ("null", "000"),
    ("M",    "001"),
    ("D",    "010"),
    ("MD",   "011"),
    ("A",    "100"),
    ("AM",   "101"),
    ("AD",   "110"),
    ("AMD",  "111")
    ]

jumpMap :: M.Map String String
jumpMap = M.fromList [
    ("null", "000"),
    ("JGT",  "001"),
    ("JEQ",  "010"),
    ("JGE",  "011"),
    ("JLT",  "100"),
    ("JNE",  "101"),
    ("JLE",  "110"),
    ("JMP",  "111")
    ]