module MyCodeGen
    ( codeGen ) where

import Sprockell (Instruction(..), RegAddr, MemAddr, AddrImmDI(..), Target(..), SprID,Operator(..), reg0, RegAddr, regA, regB, regC, regSprID, charIO,numberIO)
import MyParser (Stmt(..), Expr(..), Op(..), Type(..), fillSymbolTable)
import Data.Char

-- Global symbol table type
type GlobalSymbolTable = [(String, MemAddr)]

-- Thread info: (name, threadId, startAddress, body)
type ThreadInfo = (String, Int, Int, [Stmt])
-- threads table
type GlobalThreadsTable = [ThreadInfo]

type LocalVarStack = [GlobalSymbolTable]

-- Add local variable to head of display stack
getFirstAvailableLocalVarAddr :: LocalVarStack->Int
getFirstAvailableLocalVarAddr [] = localVarStartAddr
getFirstAvailableLocalVarAddr (x:xs) | null x = getFirstAvailableLocalVarAddr xs
  | otherwise = addr+1
  where addr = foldl (\m (_,a)-> max m a) localVarStartAddr x
  
addLocalVariable :: LocalVarStack->String->LocalVarStack
addLocalVariable st@(x:xs) name = ((name,availableAddr):x):xs
  where availableAddr = getFirstAvailableLocalVarAddr st

-- lookup display stack
getMemAddrforLocalVar :: LocalVarStack->String-> MemAddr
getMemAddrforLocalVar [] _ = error "Type checking missed a non defined variable reference ?? (never going to happen)"
getMemAddrforLocalVar (x:xs) name = case lookup name x of
    Just addr -> addr

    Nothing -> getMemAddrforLocalVar xs name

-- Returns nothing if variable is not found during lookup
-- usefull if we don't know wether the variable is local or global
getMemAddrforLocalVarMaybe :: LocalVarStack->String->Maybe MemAddr
getMemAddrforLocalVarMaybe [] _ = Nothing
getMemAddrforLocalVarMaybe (x:xs) name = case lookup name x of
    Just addr -> Just addr
    Nothing -> getMemAddrforLocalVarMaybe xs name


-- Used when encountering a new body, add a level to the display stack
newBlockInStack :: LocalVarStack->LocalVarStack
newBlockInStack s = []:s

-- Used when encountering a getting out of a body, remove a level to the display stack
popBlockFromStack :: LocalVarStack->LocalVarStack
popBlockFromStack = tail


-- Add a global variable to the symbol table
addGlobalVariable :: String -> Type -> GlobalSymbolTable -> (GlobalSymbolTable, MemAddr)
addGlobalVariable name typ table =
  let addr = case table of
              [] -> 0
              _ -> maximum (map snd table) + 1
      newTable = (name, addr) : table
  in (newTable, addr)


-- Get memory address for a given object
getMemAddrFromTable :: GlobalSymbolTable->String-> MemAddr
getMemAddrFromTable gt name =
  case lookup name gt of
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







-- Two-Pass Generation :
-- First pass fills global symbol table with locks and everything
-- Second pass generates everything to get exact thread body sizes, 
firstPassGeneration :: [Stmt] -> Int -> (GlobalSymbolTable,LocalVarStack)
firstPassGeneration [] lc = ([],[])

firstPassGeneration ((LockCreate ln):xs) lc = do 
  let (gt,st) = firstPassGeneration xs (lc+1) 
  ((ln,lockStartAddr+lc):gt,st)

firstPassGeneration ((Declaration typ name):xs) lc = do 
  let (gt,st) = firstPassGeneration xs lc
  -- different memory zones and tables for global and local variables
  case typ of
    (Global subtype)->let (gt2,_) = addGlobalVariable name subtype gt in (gt2,st)
    _->(gt,addLocalVariable st name)


-- stack in if/while/thread bodies is extended and filled right before opening it

firstPassGeneration (_:xs) lc = firstPassGeneration xs lc



-- TODO : ADD Stack for local variables passing in second pass
-- Second pass: generate all code to determine exact sizes with proper nested thread handling
secondPassGeneration :: GlobalSymbolTable -> [Stmt] -> (GlobalThreadsTable, [Instruction], Int)
secondPassGeneration gt stmts = 
    do 
    let (threads, mainBody, _) = collectAndGenerateThreads gt stmts 0 0
    let headerSize = calculateHeaderSize threads


        -- Adjust thread start addresses by adding header size
        -- we could not know actual start address as we cannot generate 
        -- header before actually iterating threads and generate their bodies
        -- to discover their sizes, 
        -- as precedent thread size obviously influence following thread start address

    let adjustedThreads = map (\(name, tid, addr, body) -> (name, tid, addr + headerSize, body)) threads
    (adjustedThreads, mainBody++[EndProg], headerSize)

-- Collect threads and generate their bodies in first pass (with thread counter)
collectAndGenerateThreads :: GlobalSymbolTable -> [Stmt] -> Int -> Int -> (GlobalThreadsTable, [Instruction], Int)

-- Base case, we don't have anymore statements from which generate code
collectAndGenerateThreads gt [] la threadCounter = ([], [], la)


collectAndGenerateThreads gt (ThreadCreate body : rest) la threadCounter = 
    do 
    let threadId = threadCounter + 1
    let threadName = "thread_" ++ show threadId ++ "_body"
        
    let joinLockMechanismSize = 8
    -- RECURSIVELY collect nested threads from the thread body
    -- Don't forget to add join counter decrement mechanism size
    -- so the calculated parent thread body length for NESTED threads 
    -- is the right one (i.e not having only the "thread logic" length as start address offset)
    let (nestedThreads, _, _) = collectAndGenerateThreads gt body (la+1+joinLockMechanismSize) threadId
        
    -- Generate the actual thread body code (including nested threads)
    let threadBodyCode = generateThreadBodyWithNested gt body nestedThreads 

    let threadSize = length threadBodyCode
    -- Process remaining statements with updated thread counter
    let maxNestedId = if null nestedThreads 
                         then threadId 
                         else maximum (map (\(_, tid, _, _) -> tid) nestedThreads)
        
    -- +1 for EndProg addition
    let (restThreads, restCode, finalAddr) = collectAndGenerateThreads gt rest (la + threadSize+1) maxNestedId
        
    -- Concat this thread, all its nested threads and all the ones in following statements
    -- la+1 for taking account of leading EndProg
    let thisThread = (threadName, threadId, la+1, body)
    let allThreads = thisThread : nestedThreads ++ restThreads
        
    -- Return updated thread table, current generated code and last address used
    (allThreads, [EndProg]++threadBodyCode ++ restCode, finalAddr)
    
collectAndGenerateThreads gt (stmt : rest) currentAddr threadCounter = 
    do 
    let stmtCode = generateStmtCode gt [] currentAddr stmt
    let stmtSize = length stmtCode 
    let (restThreads, restCode, finalAddr) = collectAndGenerateThreads gt rest (currentAddr + stmtSize) threadCounter
    
    -- Return thread table generated by following statements
    -- Current generated code and last address used
    (restThreads, stmtCode ++ restCode, finalAddr)



-- Generate thread body that can contain nested threads
generateThreadBodyWithNested :: GlobalSymbolTable -> [Stmt] -> GlobalThreadsTable -> [Instruction]
generateThreadBodyWithNested gt body nestedThreads = 
    do 
    let -- Generate the thread's own code
        ownCode = concatMap (generateStmtCodeForThread gt nestedThreads 0) body
          -- Acquire join lock, decrement variable, free join lock
          ++ [TestAndSet (DirAddr joinLockAddr), 
              Receive r1,
              Branch r1 (Rel 2), -- if 1, don't loop back
              Jump (Rel (-3)), -- if we are here, it means it was 0, so loop back
              Load (DirAddr joinLockAddr) r1,
              Compute Decr r1 r1 r1,
              WriteInstr r1 (DirAddr joinLockAddr),
              WriteInstr reg0 (DirAddr joinLockAddr)
            ]
        -- Add nested thread bodies at the end
    let nestedBodies = concatMap (\(_, _, _, nestedBody) -> 
                                   generateThreadBodyWithNested gt nestedBody []) nestedThreads
    
    -- Concat everything
    let rest = if not (null nestedBodies) then EndProg:nestedBodies else []
    ownCode ++ rest 

-- Like generateThreadBodyWithNested but for normal bodies e.g if body
-- still supports nested threads
generateNormalBodyWithNested :: GlobalSymbolTable -> [Stmt] -> GlobalThreadsTable -> [Instruction]
generateNormalBodyWithNested gt body nestedThreads = do
    -- Generate the body's own code
    let ownCode = concatMap (generateStmtCodeForThread gt nestedThreads 0) body
        -- Add nested thread bodies at the end
    let nestedBodies = concatMap (\(_, _, _, nestedBody) -> 
                                   generateThreadBodyWithNested gt nestedBody []) nestedThreads
    
    ownCode ++ nestedBodies



-- Generate statement code within a thread context (handles nested ThreadCreate)
generateStmtCodeForThread :: GlobalSymbolTable -> GlobalThreadsTable -> Int -> Stmt -> [Instruction]
generateStmtCodeForThread gt nestedThreads la (ThreadCreate body) = 
    -- When we encounter a ThreadCreate inside a thread, it should be handled
    -- by the nested thread system, so we don't generate code here
    []
generateStmtCodeForThread gt nestedThreads la stmt = 
    generateStmtCode gt [] la stmt


-- Calculate header size based on number of threads
calculateHeaderSize :: GlobalThreadsTable -> Int
calculateHeaderSize threads = do
    let numThreads = length threads
    let setupSize = numThreads * 2  -- 2 instructions per thread setup
    let jumpLogicSize = 7  -- Fixed size for jump logic
    let branchSize = 1     -- Initial branch instruction
    let joinCounter = 2
    branchSize + jumpLogicSize + setupSize + joinCounter

generateThreadJumpCode :: GlobalThreadsTable->[Instruction]
generateThreadJumpCode [] = [
        -- writeInstr WILL GO THERE
          
         Jump (Rel 7)               -- Sprockell 0 jumps to skip thread repartition
         -- beginLoop
         , ReadInstr (IndAddr regSprID)
         , Receive regA
         , Compute Equal regA reg0 regB
         , Branch regB (Rel (-3))
         , WriteInstr regA numberIO
         , Jump (Ind regA)

        -- REST OF THE PROGRAM WILL GO THERE

       ]

-- (name, threadId, startAddress, body)
generateThreadJumpCode ((name,id,sa,body):ts) = [Load (ImmValue sa) regC,WriteInstr regC (DirAddr id)]
  ++ generateThreadJumpCode ts



-- Wrapper function to put threads
buildHeader :: GlobalThreadsTable->[Instruction]
-- length tt*2 corresponds to loads + write, 
-- +2 corresponds to jump + actual target instruction
buildHeader tt = [Branch regSprID (Rel (length tt*2+2)),
  Load (ImmValue (length tt)) r1 , 
  WriteInstr r1 (DirAddr threadJoinAddr)]
  ++ generateThreadJumpCode tt



-- Generate code for a statement
generateStmtCode :: GlobalSymbolTable->LocalVarStack->GlobalThreadsTable->Int-> Stmt -> [Instruction]
generateStmtCode gt st _ _ (Declaration typ name) = 
  case typ of
    -- Variable declaration is already taken care of, 
    -- we just add initialisation to 0 for locals
    (Global Lock) -> [] -- locks are already taken care of
    (Global _) -> []
    _ -> [Load (ImmValue 0) r1,Store r1 (DirAddr $ getMemAddrforLocalVar st name)]

generateStmtCode gt st tt _ (Assignment name expr) =
  do
  let exprCode = generateExprCode gt st expr

  -- TODO : differentiate the case of local vars from global ones
  -- (Store vs WriteInstr)
  -- priority is given on local vars in case of name collision

  let result = getMemAddrforLocalVarMaybe st name
  case result of
    -- Name refers to a global variable
    Nothing -> do 
      let varAddr = getMemAddrFromTable gt name
      exprCode ++ [Pop r1,WriteInstr r1 (DirAddr varAddr)]

    -- Local one
    (Just addr)-> exprCode ++ [Pop r1,Store r1 (DirAddr addr)]

  


generateStmtCode gt st tt _ (If cond body1 body2) =
  do
    let condCode = generateExprCode gt st cond
    let elseBodyCode = generateNormalBodyWithNested gt body2 tt
    let ifBodyCode = generateNormalBodyWithNested gt body1 tt
        -- We use NOP as a fallback for condition as wReceive 2,Compute Eque don't know anything about what's after
    condCode ++ [Pop r1] ++ [Branch r1 (Rel (length elseBodyCode + 1))] -- Jump to If 
      ++ elseBodyCode
      ++[Jump (Rel (length ifBodyCode +1))] -- Jump to NOP 
      ++ ifBodyCode 
      ++ [Nop] -- Fallback for end of condition


generateStmtCode gt st tt _ (While cond body) =
  do 
    let condCode = generateExprCode gt st cond
    let bodyCode = generateNormalBodyWithNested gt body tt
    let loopStart = length bodyCode + length condCode + 4
    let loopEnd = length bodyCode + length condCode + 3

  -- same here as for condition, NOP as fallback after while
    [Jump (Rel loopStart)] 
      ++ condCode 
      ++ [Pop r1]
      ++ [Branch r1 (Rel (length bodyCode + 1)), Jump (Rel (length bodyCode + 2))] 
      ++ bodyCode ++ [Jump (Rel (-loopStart))] 
      ++ [Nop]




-- print array of numbers
-- generateStmtCode globalTable tt _ (Print (ArrayLit)) =
--   let exprCode = generateExprCode globalTable e
--   in exprCode ++ [WriteInstr r1 charIO]

-- Print number
generateStmtCode gt st tt _ (Print e) =
  let exprCode = generateExprCode gt st e
  -- writeString do not uses stack, so Pop r1 still gives us the expression value
  in exprCode ++writeString "OUT : " ++ [Pop r1,WriteInstr r1 charIO]

generateStmtCode _ _ _ _ (ThreadJoin) = [
          TestAndSet (DirAddr joinLockAddr), 
          Receive r1,
          Branch r1 (Rel 2), -- if 1, don't loop back
          Jump (Rel (-3))
          ]

generateStmtCode gt _ _ _ (LockFree lockName) =
  let lockAddr = getMemAddrFromTable gt lockName
  in [WriteInstr reg0 (DirAddr lockAddr)] -- Release lock by writing 0


generateStmtCode gt _ _ _ (LockGet lockName) =
  let lockAddr = getMemAddrFromTable gt lockName
  in [TestAndSet (DirAddr lockAddr), Receive r1,Branch r1 (Rel 2),Jump (Rel (-3))] -- Acquire lock with test-and-set


generateStmtCode gt st tt la (ScopeBlock body) = generateNormalBodyWithNested gt body tt






-- Generate code for an expression
generateExprCode :: GlobalSymbolTable->LocalVarStack-> Expr -> [Instruction]

generateExprCode _ _ (IntLit n) = [Load (ImmValue (fromIntegral n)) r1, Push r1]
-- Store booleans as int in {0,1}
generateExprCode _ _ (BoolLit b) = [Load (ImmValue (if b then 1 else 0)) r1, Push r1]

generateExprCode gt st (Var name) = do

    let result = getMemAddrforLocalVarMaybe st name
    case result of

      -- Name refers to a global variable
      Nothing -> do 
        let varAddr = getMemAddrFromTable gt name
        [Receive r1,Push r1]

      -- Local one
      (Just addr)->[Load (DirAddr addr) r1, Push r1]


generateExprCode gt st (BinOp op e1 e2) =
  do
  let e1Code = generateExprCode gt st e1
  let e2Code = generateExprCode gt st e2
  let opCode = case op of
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
  e1Code -- assume expression always push result to stack
    ++ [Pop r1] 
    ++ e2Code 
    ++ [Pop r2]
    -- Assume e1 is in r1, e2 in r2, result in r3
    ++ [Compute opCode r1 r2 r3]
    -- put result in stack
    ++ [Push r3] 



generateExprCode gt st (UnOp op e) =
  do
  let eCode = generateExprCode gt st e
  let computeCode = 
        case op of
          -- Booleans are ints in {0,1} so we need to make ifs
          -- boolean negation

          MyParser.Not -> do
                          let ifBody = [Push reg0]

                          let elseBody = [Load (ImmValue 1) r2, 
                                  Push r2,
                                  Jump (Rel (length ifBody+1))
                                  ]
                          -- assume boolean expr result is on top of stack
                          [Pop r1,Branch r1 (Rel (length elseBody+1))]
                            ++ elseBody 
                            ++ ifBody
                            ++ [Nop]
                          -- at this point, result will be on top of stack

          -- integer inversion
          MyParser.Inv -> [Compute Sprockell.Sub 0 r1 r1]

  eCode ++ computeCode

generateExprCode gt st (Paren e) = generateExprCode gt st e

generateExprCode gt st (ArrayLit exprs) =
  let exprCodes = map (generateExprCode gt st) exprs
      arrayLength = length exprs
      arrayAddr = allocateArrayMemory gt arrayLength

  -- Index of expression is also offset in array, as we assume one address point on 4 bytes ;)
  in concatMap (\ (idx, exprCode) -> exprCode ++ [Store r1 (DirAddr (arrayAddr + idx))]) (zip [0..] exprCodes)

generateExprCode gt st (ArrayAccess arrayName indexExpr) =
  let indexCode = generateExprCode gt st indexExpr
      arrayAddr = getMemAddrFromTable gt arrayName

  in indexCode ++ [Load (DirAddr (arrayAddr + r1)) r1,Push r1] -- Load array element at address arrayAddr + index


-- CODE FROM GITHUB EXAMPLES
-- | Generate code to print a (Haskell) String
writeString :: String -> [Instruction]
writeString str = concat $ map writeChar str

-- | Generate code to print a single character
writeChar :: Char -> [Instruction]
writeChar c =
    [ Load (ImmValue $ ord c) regA
    , WriteInstr regA charIO
    ]
-- END OF GITHUB EXAMPLE

-- Register and memory address management
r1, r2, r3 :: RegAddr
r1 = regA
r2 = regB
r3 = regC

threadJoinAddr :: MemAddr
threadJoinAddr = 0xdead

joinLockAddr :: MemAddr 
joinLockAddr = 0xdeae

lockStartAddr :: MemAddr
lockStartAddr = 0x10FF

localVarStartAddr :: MemAddr
localVarStartAddr = 0x1111

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

codeGen :: [Stmt] -> [Instruction]
codeGen ss = do
  let (tt,body,la) = secondPassGeneration (firstPassGeneration ss 0) ss
  let header = buildHeader tt
  header ++ body


  -- TODO : REPLACE EMPTY LIST BY SYMBOL TABLE (VARIABLE ALLOCATION)

-- TODO : local variables (register constraints), tests 

--  Branch regSprID (Rel 6) 
-- tout en haut pour éviter la partie où le thread 0 initialise les writeInstr
-- WrintrInstr registerNumLine (DirAddr AddrDeductibleFromThreadNumber)
-- the write Instr will put a value in shared memory ( the line where to jump)
-- then, for all threads except thread 0 (=> t0 jumps over that part),
-- we send a request to read that value with ReadInstr (Inaddr ..) 
-- then we wait the answer with Recieve regX
-- finally, we jump to the address in regX :)
 


-- testAndSet atomically sets a variable to 1 if it is 0.
-- if it was already 1, we get a 1 when we call Receive 
