module MyCodeGen
    ( codeGen ) where

import Sprockell (Instruction(..), RegAddr, MemAddr, AddrImmDI(..), Target(..), SprID,Operator(..), reg0, RegAddr, regA, regB, regC, regSprID)
import MyParser (Stmt(..), Expr(..), Op(..), Type(..), fillSymbolTable)

-- Global symbol table type
type GlobalSymbolTable = [(String, MemAddr)]

-- Thread info: (name, threadId, startAddress, body)
type ThreadInfo = (String, Int, Int, [Stmt])
type GlobalThreadsTable = [ThreadInfo]

-- APPROACH 1: Two-Pass Generation (Recommended)
-- First pass generates everything to get exact sizes, second pass uses real addresses

-- First pass: generate all code to determine exact sizes with proper nested thread handling
firstPassGeneration :: GlobalSymbolTable -> [Stmt] -> (GlobalThreadsTable, [Instruction], Int)
firstPassGeneration globalTable stmts = 
    let (threads, mainBody, _) = collectAndGenerateThreads globalTable stmts 0 0
        headerSize = calculateHeaderSize threads
        -- Adjust thread start addresses by adding header size
        adjustedThreads = map (\(name, tid, addr, body) -> (name, tid, addr + headerSize, body)) threads
    in (adjustedThreads, mainBody, headerSize)

-- Collect threads and generate their bodies in first pass (with thread counter)
collectAndGenerateThreads :: GlobalSymbolTable -> [Stmt] -> Int -> Int -> (GlobalThreadsTable, [Instruction], Int)
collectAndGenerateThreads gt [] currentAddr threadCounter = ([], [], currentAddr)
collectAndGenerateThreads gt (ThreadCreate body : rest) currentAddr threadCounter = 
    let threadId = threadCounter + 1
        threadName = "thread_" ++ show threadId ++ "_body"
        
        -- RECURSIVELY collect nested threads from the thread body
        (nestedThreads, nestedBody, _) = collectAndGenerateThreads gt body 0 threadId
        
        -- Generate the actual thread body code (including nested threads)
        threadBodyCode = generateThreadBodyWithNested gt body nestedThreads
        threadSize = length threadBodyCode
        
        -- Process remaining statements with updated thread counter
        let maxNestedId = if null nestedThreads 
                         then threadId 
                         else maximum (map (\(_, tid, _, _) -> tid) nestedThreads)
        (restThreads, restCode, finalAddr) = collectAndGenerateThreads gt rest (currentAddr + threadSize) maxNestedId
        
        -- Add this thread and all its nested threads
        thisThread = (threadName, threadId, currentAddr, body)
        allThreads = thisThread : nestedThreads ++ restThreads
        
    in (allThreads, threadBodyCode ++ restCode, finalAddr)
    
collectAndGenerateThreads gt (stmt : rest) currentAddr threadCounter = 
    let stmtCode = generateStmtCode gt [] currentAddr stmt
        stmtSize = length stmtCode
        (restThreads, restCode, finalAddr) = collectAndGenerateThreads gt rest (currentAddr + stmtSize) threadCounter
    in (restThreads, stmtCode ++ restCode, finalAddr)

-- Generate thread body that can contain nested threads
generateThreadBodyWithNested :: GlobalSymbolTable -> [Stmt] -> GlobalThreadsTable -> [Instruction]
generateThreadBodyWithNested gt body nestedThreads = 
    let -- Generate the thread's own code
        ownCode = concatMap (generateStmtCodeForThread gt nestedThreads 0) body
        -- Add nested thread bodies at the end
        nestedBodies = concatMap (\(_, _, _, nestedBody) -> 
                                   generateThreadBodyWithNested gt nestedBody []) nestedThreads
    in ownCode ++ nestedBodies ++ [EndProg]

-- Generate statement code within a thread context (handles nested ThreadCreate)
generateStmtCodeForThread :: GlobalSymbolTable -> GlobalThreadsTable -> Int -> Stmt -> [Instruction]
generateStmtCodeForThread gt nestedThreads addr (ThreadCreate body) = 
    -- When we encounter a ThreadCreate inside a thread, it should be handled
    -- by the nested thread system, so we don't generate code here
    []
generateStmtCodeForThread gt nestedThreads addr stmt = 
    generateStmtCode gt [] addr stmt

-- Calculate header size based on number of threads
calculateHeaderSize :: GlobalThreadsTable -> Int
calculateHeaderSize threads = 
    let numThreads = length threads
        setupSize = numThreads * 2  -- 2 instructions per thread setup
        jumpLogicSize = 5  -- Fixed size for jump logic
        branchSize = 1     -- Initial branch instruction
    in branchSize + jumpLogicSize + setupSize
