module Parser (
    extractLabels,
    purify
) where

import Data.Char
import Data.List (isPrefixOf)

import qualified Types as T
import qualified Data.Map as M

-- exposed functions

extractLabels :: [String] -> T.LabelMap
extractLabels contentLines = M.fromList $ collectLabels contentLines 0 0
  where
    collectLabels [] _ _ = []
    collectLabels (line:lines) lineNumber instructionAddress
        | isLabel line = case parseLabel line of
            Just label -> (label, instructionAddress) : collectLabels lines (lineNumber + 1) instructionAddress
            Nothing -> collectLabels lines (lineNumber + 1) instructionAddress
        | otherwise = collectLabels lines (lineNumber + 1) (instructionAddress + 1)

    isLabel line = head line == '(' && last line == ')'

purify :: [String] -> [String]
purify = filter (not . isCommentOrEmpty) . map trim
    where isCommentOrEmpty line = isComment line || isEmptyLine line

-- private and helper functions

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
