module Types (
    Label,
    LineNumber,
    SymbolMap
) where

import qualified Data.Map as M

-- common types used across multiple assembler modules
type Label = String
type LineNumber = Int
type SymbolMap = M.Map Label LineNumber