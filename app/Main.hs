module Main where

import MyParser (parseMyLang)
import MyCodeGen (codeGen)
import Sprockell (regA,regB,regC,regD,regE,runWithDebugger,debuggerSimplePrint,DbgInput,localMem,sprStates, Instruction(..), RegAddr, MemAddr, AddrImmDI(..), Target(..), SprID,Operator(..), reg0, RegAddr, regA, regB, regC, regSprID, charIO, run)
import Data.Char
-- Compiles a number into a spril program producing all fibonacci numbers below the number
-- Compilation might fail
--compile :: String -> Either String [Instruction]
compile = 1
-- compile txt = do
    -- ast <- parseMyLang txt

    -- -- --parseMyLang txt
    -- case ast of
    --      (Left err) -> error "Malformed program"
    --      (Right p) -> pure $ codeGen p

prog :: [Instruction]
prog =
    writeString "What is your name? " ++
    [ Load (ImmValue $ ord '\n') regE   -- ASCII code newline in regE for later reference

    -- "beginInputLoop": 39
    , ReadInstr charIO                  -- Request a character from stdin
    , Receive regA                      -- Save it in regA (as ASCII code)
    , Branch regA (Rel 2)
    , Jump (Rel (-3))                   -- got 0, no input available, try again

    -- got input char
    , Compute Equal regA regE regC      -- check if it's a newline (remember: in regE)
    , Branch regC (Rel 4)               -- then jump to "inputDone"
    , Store regA (IndAddr regB)         -- else store character in local memory
    , Compute Incr regB regB regB
    , Jump (Rel (-8))                   -- "beginInputLoop"
    ]
    -- "inputDone"
    ++ writeString "Hello "
    ++
    -- "beginLoopOutput"
    [ Load (IndAddr regD) regA
    , WriteInstr regA charIO
    , Compute Incr regD regD regD
    , Compute NEq regB regD regC
    , Branch regC (Rel (-4))            -- target "loopOutput"
    ]
    ++ writeString "!\n"
    ++ [EndProg]


-- | Generate code to print a (Haskell) String
writeString :: String -> [Instruction]
writeString str = concat $ map writeChar str

-- | Generate code to print a single character
writeChar :: Char -> [Instruction]
writeChar c =
    [ Load (ImmValue $ ord c) regA
    , WriteInstr regA charIO
    ]
-- Gets a number and runs the resulting spril program of compilation succeeds
main :: IO ()
main = --pure ()
-- main = do
--     txt <- getLine
--     case compile txt of
--          (Left err) -> fail err
--          (Right spril) -> run [spril]
    do 
        let prog1 = [Branch 1 (Rel 2),Load (ImmValue 0) 2,WriteInstr 2 (DirAddr 57005),Jump (Rel 7),ReadInstr (IndAddr 1),Receive 2,Compute Equal 2 0 3,Branch 3 (Rel (-3)),WriteInstr 2 (DirAddr 65536),Jump (Ind 2),Load (ImmValue 5) 2,Push 2,Load (ImmValue 79) 2,WriteInstr 2 (DirAddr 65537),Load (ImmValue 85) 2,WriteInstr 2 (DirAddr 65537),Load (ImmValue 84) 2,WriteInstr 2 (DirAddr 65537),Load (ImmValue 32) 2,WriteInstr 2 (DirAddr 65537),Load (ImmValue 58) 2,WriteInstr 2 (DirAddr 65537),Load (ImmValue 32) 2,WriteInstr 2 (DirAddr 65537),Pop 2,WriteInstr 2 (DirAddr 65536),EndProg]
        let prog2 = [Branch 1 (Rel 2),Load (ImmValue 0) 2,WriteInstr 2 (DirAddr 57005),Jump (Rel 7),ReadInstr (IndAddr 1),Receive 2,Compute Equal 2 0 3,Branch 3 (Rel (-3)),WriteInstr 2 (DirAddr 65536),Jump (Ind 2),Load (ImmValue 1) 2,Push 2,Pop 2,Branch 2 (Rel 18),Load (ImmValue 10) 2,Push 2,Load (ImmValue 79) 2,WriteInstr 2 (DirAddr 65537),Load (ImmValue 85) 2,WriteInstr 2 (DirAddr 65537),Load (ImmValue 84) 2,WriteInstr 2 (DirAddr 65537),Load (ImmValue 32) 2,WriteInstr 2 (DirAddr 65537),Load (ImmValue 58) 2,WriteInstr 2 (DirAddr 65537),Load (ImmValue 32) 2,WriteInstr 2 (DirAddr 65537),Pop 2,WriteInstr 2 (DirAddr 65536),Jump (Rel 17),Load (ImmValue 5) 2,Push 2,Load (ImmValue 79) 2,WriteInstr 2 (DirAddr 65537),Load (ImmValue 85) 2,WriteInstr 2 (DirAddr 65537),Load (ImmValue 84) 2,WriteInstr 2 (DirAddr 65537),Load (ImmValue 32) 2,WriteInstr 2 (DirAddr 65537),Load (ImmValue 58) 2,WriteInstr 2 (DirAddr 65537),Load (ImmValue 32) 2,WriteInstr 2 (DirAddr 65537),Pop 2,WriteInstr 2 (DirAddr 65536),Nop,EndProg]
        -- Sprockell.runWithDebugger (debuggerSimplePrint showLocalMem) [prog]
        Sprockell.run [prog2]

showLocalMem :: DbgInput -> String
showLocalMem ( _ , systemState ) = show $ localMem $ head $ sprStates systemState
