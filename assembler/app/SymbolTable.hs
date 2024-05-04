module SymbolTable (
    createSymbolTable,
    add,
    getRegister
) where

import qualified Data.Map as M
import qualified Types as T

createSymbolTable :: T.SymbolTable
createSymbolTable = M.fromList $ createRIntRegisters ++ [
    ("SP", 0),  -- stack pointer, initially pointing to 0
    ("LCL", 1), -- Local Segmet Pointer: pointing to base register for local variables
    ("ARG", 2), -- Argument Segment Pointer: pointing towards base register of function arguments
    ("THIS", 3),    -- This Segmet Pointer: pointing towards current object (this)
    ("THAT", 4),    -- That Segmet Pointer: pointing towards other object (any)
    ("SCREEN", 16384),  -- starting address of screen addresses
    ("KBD", 24576) -- address of current state of keyboard
    ]

add :: T.SymbolTable -> T.Symbol -> T.SymbolTable
add currentTable newSymbol = 
    let maxReg = maximum . filter (< 16384) . M.elems $ currentTable
        nextAddress = maxReg + 1
    in M.insert newSymbol nextAddress currentTable

getRegister :: T.SymbolTable -> T.Symbol -> Maybe T.Address
getRegister table symbol = M.lookup symbol table

-- non-exposed functions

-- create R0, 0 ... R15, 15
createRIntRegisters :: [(T.Symbol, T.Address)]
createRIntRegisters = [("R" ++ show i, i) | i <- [0..15]]