module Parser (
    parse
) where

parse :: String -> String
parse contents = unlines $ parseContents (lines contents) []

parseContents :: [String] -> [String] -> [String]
parseContents [] parsed = parsed
parseContents (line:lines) parsed = parseContents lines (parsed ++ [newLineContent])
    newLineContent = line

