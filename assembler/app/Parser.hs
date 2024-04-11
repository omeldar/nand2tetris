module Parser (
    parse
) where

parse :: String -> String
parse contents = parseContents (lines contents) []

parseContents :: [String] -> [String]
parseContents [] parsed = parsed
parseContents lines parsed = 

