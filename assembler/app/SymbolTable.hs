module SymbolTable (
    SymbolTable
) where

import qualified Data.Map as Map

type SymbolTable = Map.Map String Int
