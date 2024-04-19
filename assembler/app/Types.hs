module Types (
    Label,
    LineNumber,
    SymbolMap
) where

import qualified Data.Map as M

-- common types used across multiple assembler modules
type Label = String
type LineNumber = Int
type Symbol = String
type Address = Int
type SymbolTable = M.Map Symbol Address