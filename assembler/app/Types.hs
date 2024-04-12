module Types (
    Label,
    LineNumber,
    SymbolTable
) where

import qualified Data.Map as M

-- common types used across multiple assembler modules
type Label = String
type LineNumber = Int
type SymbolTable = M.Map Label LineNumber