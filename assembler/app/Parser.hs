module Parser (
    extractLabels,
    purify
) where

import Data.Maybe (catMaybes)
import Data.Char
import Data.List (isPrefixOf)

import qualified Types as T
import qualified Data.Map as M

-- exposed functions

extractLabels :: [String] -> T.SymbolMap
extractLabels lines =
    let labeledLines = catMaybes $ zipWith (\lineNumber line -> fmap (\(label, _) -> (label, lineNumber + 1)) (parseLabel line)) [1..] lines
    in M.fromList labeledLines

purify :: [String] -> [String]
purify = filter (not . isCommentOrEmpty)
    where isCommentOrEmpty line = isComment line || isEmptyLine line

-- private and helper functions

parseLabel :: String -> Maybe (T.Label, T.LineNumber)
parseLabel line = case words line of
    ['(':label] -> Just (init label, 0)
    _ -> Nothing

isComment :: String -> Bool
isComment line = "//" `isPrefixOf` dropWhile isSpace line

isEmptyLine :: String -> Bool
isEmptyLine = all isSpace
