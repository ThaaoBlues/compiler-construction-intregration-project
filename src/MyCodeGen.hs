module MyCodeGen
    ( codeGen ) where

import Sprockell (Instruction(..), RegAddr, MemAddr, AddrImmDI(..), Target(..), SprID,Operator(..), reg0, RegAddr, regA, regB, regC, regSprID)
import MyParser (Stmt(..), Expr(..), Op(..), Type(..), fillSymbolTable)

-- Global symbol table type
type GlobalSymbolTable = [(String, MemAddr)]

-- Thread info: (name, threadId, startAddress, body)
type ThreadInfo = (String, Int, Int, [Stmt])
-- threads table
type GlobalThreadsTable = [ThreadInfo]

addThread :: Int->GlobalThreadsTable -> GlobalThreadsTable
addThread la tt= tt 
  ++ [("thread_"++show tid++"_body",tid,la+1)]    
  where tid = length tt +1

generateThreadJumpCode :: GlobalThreadsTable->[Instruction]
generateThreadJumpCode [] = [
        -- writeInstr WILL GO THERE

         Jump (Rel 5)               -- Sprockell 0 jumps to skip thread repartition
         -- beginLoop
         , ReadInstr (IndAddr regSprID)
         , Receive regA
         , Compute Equal regA reg0 regB
         , Branch regB (Rel (-3))

        -- REST OF THE PROGRAM WILL GO THERE

        -- endLoop
        --  , WriteInstr regA numberIO
        --  , Jump (Ind regA)

        --  -- 12: Sprockell 0 is sent here
        --  , EndProg

        --  -- 13: Sprockells 1, 2 and 3 are sent here
        --  , EndProg
       ]
generateThreadJumpCode (t:ts) = WriteInstr regC (DirAddr (length ts)):generateThreadJumpCode ts



-- Initial global symbol table
globalSymbolTable :: GlobalSymbolTable
globalSymbolTable = []

-- Add a global variable to the symbol table
addGlobalVariable :: String -> Type -> GlobalSymbolTable -> (GlobalSymbolTable, MemAddr)
addGlobalVariable name typ table =
  let addr = case table of
              [] -> 0
              _ -> maximum (map snd table) + 1
      newTable = (name, addr) : table
  in (newTable, addr)


-- Get memory address for a given object
getMemAddrFromTable :: String -> GlobalSymbolTable -> MemAddr
getMemAddrFromTable name table =
  case lookup name table of
    Just addr -> addr
    Nothing -> error ("Global variable not found: " ++ name)

-- Add a lock to the symbol table
addLock :: String -> GlobalSymbolTable -> (GlobalSymbolTable, MemAddr)
addLock name table =
  let addr = case table of
              [] -> 0
              _ -> maximum (map snd table) + 1
      newTable = (name, addr) : table
  in (newTable, addr)


-- Allocate memory for an array
allocateArrayMemory :: GlobalSymbolTable -> Int -> MemAddr
allocateArrayMemory globalTable length =
  let maxAddr = case globalTable of
                 [] -> 0
                 _ -> maximum (map snd globalTable) + 1 -- [(varname,memory_addr)]
  in maxAddr



-- Wrapper function to put threads
codeGen :: [Stmt]->[Instruction]

codeGen ss = do
    (body,header) <- generateCode [] []  ss 0
    header ++ body



-- Generate code for a list of statements
-- the int is there to get the last address of the currently generated code
generateCode :: GlobalSymbolTable->GlobalThreadsTable-> [Stmt]->Int -> ([Instruction],[Instruction])

-- No more statements, we can then generate the program header that manage jumps etc..

generateCode gt tt [] la 
  | null tt = []
  | otherwise = do
    let jumpCode = generateThreadJumpCode tt
    -- Jump length tt+1 as it will corresspond to the first non-t0 exclusive part
    -- (the Read-receive-jump part)
    -- as there is as many WriteInstr lines as there are threads 
    Branch regSprID (Rel (length tt +1)):jumpCode
  

-- Fills threads table to use it while building the jump section
generateCode gt tt (s@(ThreadCreate body):xs) la = do
  let ntt = addThread la tt
  let code = generateStmtCode gt tt la s
  code ++ do 
    
    (body,header) <- generateCode gt ntt xs (la+length code)
    header ++ body


-- Default behavior
generateCode gt tt (s:xs) la = 
    code ++ generateCode gt tt xs (la+length code)
    where code = generateStmtCode gt tt la s
    

-- Generate code for a statement
generateStmtCode :: GlobalSymbolTable->GlobalThreadsTable->Int-> Stmt -> [Instruction]
generateStmtCode globalTable _ _ (Declaration typ name) =
  case typ of
    (Global _) -> let (newGlobalTable, addr) = addGlobalVariable name typ globalTable
                  in []
    _ -> [] -- For local variables, we need to manage registers


generateStmtCode globalTable tt _ (Assignment var expr) =
  let exprCode = generateExprCode globalTable expr
      varAddr = getMemAddrFromTable var globalTable
  in exprCode ++ [Store r1 (DirAddr varAddr)]


generateStmtCode globalTable tt la (If cond body1 body2) =
  let condCode = generateExprCode globalTable cond
      elseBodyCode = generateCode globalTable tt body1 (la + length condCode + 1)
      ifBodyCode = generateCode globalTable tt body1 (la+ length condCode+1+length elseBodyCode+1)
      condReg = r1 -- Assume condition is in r1
      -- We use NOP as a fallback for condition as we don't know anything about what's after
  in condCode ++ [Branch condReg (Rel (length elseBodyCode + 2))] -- Jump to If 
  ++ elseBodyCode
  ++[Jump (Rel (length ifBodyCode +1))] -- Jump to NOP 
  ++ ifBodyCode 
  ++ [Nop] -- Placeholder for end of condition


generateStmtCode globalTable tt la (While cond body) =
  do 
    let condCode = generateExprCode globalTable cond
    let bodyCode = generateCode globalTable tt body (la+length condCode+3)  
    let condReg = r1 -- Assume condition is in r1
    let loopStart = length bodyCode + length condCode + 3
    let loopEnd = length bodyCode + length condCode + 2

  -- same here as for condition, NOP as fallback after while
    [Jump (Rel loopStart)] ++ condCode ++ [Branch condReg (Rel (length bodyCode + 1)), Jump (Rel (length bodyCode + 2))] ++ bodyCode ++ [Jump (Rel (-loopStart))] ++ [Nop]


generateStmtCode globalTable tt _ (Print expr) =
  let exprCode = generateExprCode globalTable expr
  in exprCode ++ [WriteInstr r1 (DirAddr outputAddress)]


generateStmtCode gt tt la tc@(ThreadCreate body) =
  do 
    let bodyCode = generateCode gt tt body la
    
    bodyCode ++ [EndProg]


-- IDEA : each thread sends a 1 value when exiting
-- Thus we can copy the loop used in the demo to wait for end of thread
generateStmtCode globalTable tt _ (ThreadJoin) = [] -- To implement: synchronization to wait for threads

generateStmtCode globalTable tt _ (StartThread threadName) = [] -- To implement: start a specific thread

generateStmtCode globalTable tt _ (LockCreate lockName) =
  let (newGlobalTable, addr) = addLock lockName globalTable
  in []


generateStmtCode globalTable tt _ (LockFree lockName) =
  let lockAddr = getMemAddrFromTable lockName globalTable
  in [WriteInstr 0 (DirAddr lockAddr)] -- Release lock by writing 0


generateStmtCode globalTable tt _ (LockGet lockName) =
  let lockAddr = getMemAddrFromTable lockName globalTable
  in [TestAndSet (DirAddr lockAddr), Receive r1] -- Acquire lock with test-and-set


generateStmtCode globalTable tt la (ScopeBlock body) = generateCode globalTable tt body la






-- Generate code for an expression
generateExprCode :: GlobalSymbolTable -> Expr -> [Instruction]

generateExprCode globalTable (IntLit n) = [Load (ImmValue (fromIntegral n)) r1] -- Assume we use r1 for the result

-- store booleans as int in {0,1}
generateExprCode globalTable (BoolLit b) = [Load (ImmValue (if b then 1 else 0)) r1]

generateExprCode globalTable (Var varName) =
  let varAddr = getMemAddrFromTable varName globalTable
  in [Load (DirAddr varAddr) r1]


generateExprCode globalTable (BinOp op e1 e2) =
  let e1Code = generateExprCode globalTable e1
      e2Code = generateExprCode globalTable e2
      opCode = case op of
        MyParser.Add -> Sprockell.Add
        MyParser.Sub -> Sprockell.Sub
        MyParser.Mul -> Sprockell.Mul
        MyParser.Eq -> Sprockell.Equal
        MyParser.Lt -> Sprockell.Lt
        MyParser.Leq -> Sprockell.LtE
        MyParser.Gt -> Sprockell.Gt
        MyParser.Geq -> Sprockell.GtE
        MyParser.Neq -> Sprockell.NEq
        MyParser.And -> Sprockell.And
        MyParser.Or -> Sprockell.Or
        _ -> error "Unsupported operation"
  in e1Code ++ [Store r1 (DirAddr tempAddr1)] ++ e2Code ++ [Load (DirAddr tempAddr1) r2, Compute opCode r1 r2 r3] -- Assume e1 is in r1, e2 in r2, result in r3



generateExprCode globalTable (UnOp op e) =
  let eCode = generateExprCode globalTable e
      computeCode = case op of

                -- Booleans are ints in {0,1} so we need to make ifs
                -- boolean negation

                MyParser.Not -> do
                                let ifBody = [Push reg0]
                                let elseBody = [Load (ImmValue 1) r2, 
                                        Push r2,
                                        Jump (Rel (length ifBody+1))
                                        ]
                                [Branch r1 (Rel (length elseBody+2))]
                                  ++ elseBody 
                                  ++ ifBody
                                  ++ [Nop]
                                  ++ [Pop r1]

                -- integer inversion
                MyParser.Inv -> [Compute Sprockell.Sub 0 r1 r1]
  in eCode ++ computeCode

generateExprCode globalTable (Paren e) = generateExprCode globalTable e

generateExprCode globalTable (ArrayLit exprs) =
  let exprCodes = map (generateExprCode globalTable) exprs
      arrayLength = length exprs
      arrayAddr = allocateArrayMemory globalTable arrayLength

  -- index of expression is also offset in array/4, as we assume everything stored on 4 bytes ;)
  in concatMap (\ (idx, exprCode) -> exprCode ++ [Store r1 (DirAddr (arrayAddr + idx*4))]) (zip [0..] exprCodes)

generateExprCode globalTable (ArrayAccess arrayName indexExpr) =
  let indexCode = generateExprCode globalTable indexExpr
      arrayAddr = getMemAddrFromTable arrayName globalTable
  in indexCode ++ [Load (DirAddr (arrayAddr + r1*4)) r1] -- Load array element at address arrayAddr + index




-- Register and memory address management
r1, r2, r3 :: RegAddr
r1 = regA
r2 = regB
r3 = regC

tempAddr1 :: MemAddr
tempAddr1 = 0xFFFE -- Temporary address for storing intermediate values

outputAddress :: MemAddr
outputAddress = 0xFFFF -- Fixed address for output operations

-- Generate a full program with multiple Sprockells
--generateFullProgram :: GlobalSymbolTable -> [Stmt] -> [[Instruction]]
--generateFullProgram = generateCode

-- main :: IO ()
-- main = do
--   let programText = unlines [
--         "global entero a:)",
--         "esclusa lock1:)",
--         "a = 5:)",
--         "imprimir ¡a!:)",
--         "hilo {",
--         "  obtener lock1:)",
--         "  a = a + 1:)",
--         "  liberar lock1:)",
--         "}",
--         "esperamos:)"
--        ]
--   case parseMyLang programText of
--     Left err -> print err
--     Right program -> do
--       let initialGlobalTable = []
--           (globalTable, _) = foldl (\(table, _) stmt ->
--                                     case stmt of
--                                       Declaration (Global _) name -> addGlobalVariable name (Global Entero) table
--                                       LockCreate name -> addLock name table
--                                       _ -> (table, ()))
--                                   (initialGlobalTable, ())
--                                   program
--           instructions = generateCode globalTable program
--       putStrLn "Generated Sprockell Instructions:"
--       mapM_ print instructions


-- TODO : thread execution,thread join, local variables (register constraints), tests 
-- INTEGRATE OUR TWO PASS SOLUTION AND PLUG IT WITH HEADER GENERATION
-- threads handling :
-- don't forget EndProg at each thread end


--  Branch regSprID (Rel 6) 
-- tout en haut pour éviter la partie où le thread 0 initialise les writeInstr
-- WrintrInstr registerNumLine (DirAddr AddrDeductibleFromThreadNumber)
-- the write Instr will put a value in shared memory ( the line where to jump)
-- then, for all threads except thread 0 (=> t0 jumps over that part),
-- we send a request to read that value with ReadInstr (Inaddr ..) 
-- then we wait the answer with Recieve regX
-- finally, we jump to the address in regX :)
 