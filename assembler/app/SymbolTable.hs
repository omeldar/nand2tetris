module SymbolTable (
    createSymbolTable
) where

import qualified Data.Map as M

type Symbol = String
type Address = Int
type SymbolTable = M.Map Symbol Address

createSymbolTable :: SymbolTable
createSymbolTable = M.fromList $ createRIntRegisters ++ [
    ("SP", 0),  -- stack pointer, initially pointing to 0
    ("LCL", 1), -- Local Segmet Pointer: pointing to base register for local variables
    ("ARG", 2), -- Argument Segment Pointer: pointing towards base register of function arguments
    ("THIS", 3),    -- This Segmet Pointer: pointing towards current object (this)
    ("THAT", 4),    -- That Segmet Pointer: pointing towards other object (any)
    ("SCREEN", 16384),  -- starting address of screen addresses
    ("KBD", 24576) -- address of current state of keyboard
    ]

-- create R0, 0 ... R15, 15
createRIntRegisters :: [(Symbol, Address)]
createRIntRegisters = [("R" ++ show i, i) | i <- [0..15]]