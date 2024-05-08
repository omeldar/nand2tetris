module Code (
    translate
) where

import Data.Char (isDigit)

import qualified Types as T
import qualified Data.Map as M

translate :: T.SymbolTable -> T.LabelMap -> [String] -> [String] -> [String]
translate _ _ [] binary = binary
translate symbols labels (line:rest) binary = translate symbols labels rest newBinary
    where
        newBinary = binary ++ [translateLine symbols labels line]

translateLine :: T.SymbolTable -> T.LabelMap -> String -> String
translateLine symbols labels line
    | isAInstruction line = translateAInstruction labels line
    | otherwise = "c-instr" -- translateCInstruction symbols labels line  -- when implemented
    where
        isAInstruction :: String -> Bool
        isAInstruction ('@':_) = True
        isAInstruction _ = False

translateAInstruction :: T.LabelMap -> String -> String
translateAInstruction labels line = case parseAInstruction labels line of
    Just address -> intToBinary address
    Nothing -> error "Invalid A-instruction format"

parseAInstruction :: T.LabelMap -> String -> Maybe Int
parseAInstruction labels ('@':rest)
    | all isDigit rest = Just (read rest) -- if is just int value, return int value
    | otherwise = M.lookup rest labels -- if is not just int value, lookup address in labels
parseAInstruction _ _ = Nothing

intToBinary :: Int -> String
intToBinary x = replicate (15 - length bin) '0' ++ bin
    where
        bin = toBinary x


toBinary :: Int -> String
toBinary 0 = ""
toBinary n = toBinary (n `div` 2) ++ show (n `mod` 2)