module Main where

import MyParser (parseMyLang,stackChecking,fillSymbolTable,fillGlobalSymbolTable, Stmt (ThreadCreate))
import MyCodeGen (codeGen)
import Sprockell (regA,regB,regC,regD,regE,runWithDebugger,debuggerSimplePrint,DbgInput,localMem,sprStates, Instruction(..), RegAddr, MemAddr, AddrImmDI(..), Target(..), SprID,Operator(..), reg0, RegAddr, regA, regB, regC, regSprID, charIO, run)
import Data.Char
import System.Environment (getArgs)
import GHC.IO.Encoding (setLocaleEncoding, utf8)

main :: IO ()
main = do 
    [fname] <- getArgs
    runFromFile fname


countThreads :: [Stmt]->Int
countThreads [] = 0
countThreads ((ThreadCreate body):xs) = 1+countThreads xs+countThreads body
countThreads (_:xs) = countThreads xs

runFromFile :: String->IO ()
runFromFile fname = do
    setLocaleEncoding utf8
    input <- readFile fname
    case parseMyLang input of
        Left err -> error (show err)
        Right ast -> do 
                        let tcr = stackChecking ast (fillGlobalSymbolTable ast) [fillSymbolTable ast] in
                            if null tcr 
                                then do 
                                    -- determine the number of threads in the program
                                    -- so we can run it on n+1 sprockells
                                    -- n+1 as we still have the main thread to run
                                    let threadsNumber = countThreads ast
                                    print ("Found "++(show threadsNumber)++" threads.")
                                    let prog = codeGen ast in run [prog | i <- [0..threadsNumber]]
                                else print $ show tcr
        
