module Parser (
    extractLabels,
    purify,
    removeLabels
) where

import Data.Char
import Data.List (isPrefixOf)

import qualified Types as T
import qualified Data.Map as M

-- exposed functions

extractLabels :: [String] -> T.LabelMap
extractLabels contentLines = M.fromList $ collectLabels contentLines 0 0
  where
    collectLabels :: [String] -> Int -> Int -> [(T.Label, Int)]
    collectLabels [] _ _ = []
    collectLabels (line:rest) lineNumber instructionAddress
        | isLabel line = case parseLabel (trim line) of
            Just label -> (label, instructionAddress) : collectLabels rest (lineNumber + 1) instructionAddress
            Nothing -> error $ "Parsing label failed for line: " ++ line
        | otherwise = collectLabels rest (lineNumber + 1) (instructionAddress + 1)

purify :: [String] -> [String]
purify = filter (not . isCommentOrEmpty) . map trim
    where
        isCommentOrEmpty line = isComment line || isEmptyLine line

removeLabels :: [String] -> [String]
removeLabels = filter (not . isLabel) . map trim

-- private and helper functions

isLabel :: [Char] -> Bool
isLabel line = head line == '(' && last line == ')'

parseLabel :: String -> Maybe T.Label
parseLabel line
    | head line == '(' && last line == ')' = Just (init (tail line))  -- Remove '(' and ')'
    | otherwise = Nothing

isComment :: String -> Bool
isComment line = "//" `isPrefixOf` dropWhile isSpace line

isEmptyLine :: String -> Bool
isEmptyLine = all isSpace

trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace
