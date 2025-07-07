module Main where

import MyParser (parseMyLang,stackChecking,fillSymbolTable,fillGlobalSymbolTable)
import MyCodeGen (codeGen)
import Sprockell (regA,regB,regC,regD,regE,runWithDebugger,debuggerSimplePrint,DbgInput,localMem,sprStates, Instruction(..), RegAddr, MemAddr, AddrImmDI(..), Target(..), SprID,Operator(..), reg0, RegAddr, regA, regB, regC, regSprID, charIO, run)
import Data.Char
import System.Environment (getArgs)
import GHC.IO.Encoding (setLocaleEncoding, utf8)

main :: IO ()
main = do 
    [fname] <- getArgs
    runFromFile fname


runFromFile :: String->IO ()
runFromFile fname = do
    setLocaleEncoding utf8
    input <- readFile fname
    case parseMyLang input of
        Left err -> error (show err)
        --Right ast -> let prog = codeGen ast in runWithDebugger (debuggerSimplePrint showGlobalMem) [prog,prog,prog]
        Right ast -> do 
                        print ast
                        let tcr = stackChecking ast (fillGlobalSymbolTable ast) [fillSymbolTable ast] in
                            if null tcr 
                                then let prog = codeGen ast in run [prog,prog,prog,prog]
                                else print $ show tcr
        
        --Right ast -> print ast