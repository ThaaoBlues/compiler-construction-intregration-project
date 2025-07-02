module MyCodeGen
    ( codeGen ) where

import Sprockell (Instruction(..), RegAddr, MemAddr, AddrImmDI(..), Target(..), SprID,Operator(..), reg0, RegAddr, regA, regB, regC, regSprID, charIO,numberIO)
import MyParser (Stmt(..), Expr(..), Op(..), Type(..), fillSymbolTable)
import Data.Char
import Test.QuickCheck.Text (number)

-- global symbol table type
type GlobalSymbolTable = [(String, MemAddr)]

-- thread info: (name, threadId, startAddress, body)
type ThreadInfo = (Int, Int, [Stmt])
-- threads table
type GlobalThreadsTable = [ThreadInfo]

type LocalVarStack = [GlobalSymbolTable]

-- add local variable to head of display stack
getFirstAvailableLocalVarAddr :: LocalVarStack->Int
getFirstAvailableLocalVarAddr [] = localVarStartAddr
getFirstAvailableLocalVarAddr (x:xs) | null x = getFirstAvailableLocalVarAddr xs
  | otherwise = addr+1
  where addr = foldl (\m (_,a)-> max m a) localVarStartAddr x
  
addLocalVariable :: LocalVarStack->String->LocalVarStack
addLocalVariable [] name = [[(name,localVarStartAddr)]]
addLocalVariable st@(x:xs) name = ((name,availableAddr):x):xs
  where availableAddr = getFirstAvailableLocalVarAddr st

-- lookup display stack
getMemAddrforLocalVar :: LocalVarStack->String-> MemAddr
getMemAddrforLocalVar [] _ = error "Type checking missed a non defined variable reference ?? (never going to happen)"
getMemAddrforLocalVar (x:xs) name = case lookup name x of
    Just addr -> addr

    Nothing -> getMemAddrforLocalVar xs name

-- returns nothing if variable is not found during lookup
-- usefull if we don't know wether the variable is local or global
getMemAddrforLocalVarMaybe :: LocalVarStack->String->Maybe MemAddr
getMemAddrforLocalVarMaybe [] _ = Nothing
getMemAddrforLocalVarMaybe (x:xs) name = case lookup name x of
    Just addr -> Just addr
    Nothing -> getMemAddrforLocalVarMaybe xs name


-- used when encountering a new body, add a level to the display stack
newBlockInStack :: LocalVarStack->LocalVarStack
newBlockInStack s = []:s

-- used when encountering a getting out of a body, remove a level to the display stack
popBlockFromStack :: LocalVarStack->LocalVarStack
popBlockFromStack = tail


-- add a global variable to the symbol table
addGlobalVariable :: String -> Type -> GlobalSymbolTable -> (GlobalSymbolTable, MemAddr)
addGlobalVariable name typ table =
  let addr = case table of
              [] -> 0
              _ -> maximum (map snd table) + 1
      newTable = (name, addr) : table
  in (newTable, addr)


-- get memory address for a given object
getMemAddrFromTable :: GlobalSymbolTable->String-> MemAddr
getMemAddrFromTable gt name =
  case lookup name gt of
    Just addr -> addr
    Nothing -> error ("Global variable not found: " ++ name)

-- add a lock to the symbol table
addLock :: String -> GlobalSymbolTable -> (GlobalSymbolTable, MemAddr)
addLock name table =
  let addr = case table of
              [] -> 0
              _ -> maximum (map snd table) + 1
      newTable = (name, addr) : table
  in (newTable, addr)


-- allocate memory for an array
allocateArrayMemory :: GlobalSymbolTable -> Int -> MemAddr
allocateArrayMemory globalTable length =
  let maxAddr = case globalTable of
                 [] -> 0
                 _ -> maximum (map snd globalTable) + 1 -- [(varname,memory_addr)]
  in maxAddr







-- two-Pass Generation :
-- first pass fills global symbol table with locks and everything
-- second pass generates everything to get exact thread body sizes, 
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

-- like first pass but only for local variables,
-- to be used when dynamcally extending stack for while/if bodies
fillLocalDisplayForThisBody :: LocalVarStack->[Stmt] -> LocalVarStack
fillLocalDisplayForThisBody st [] = st 
fillLocalDisplayForThisBody st ((Declaration typ name):xs) = addLocalVariable st2 name
  where st2 = fillLocalDisplayForThisBody st xs 
fillLocalDisplayForThisBody st (x:xs) = fillLocalDisplayForThisBody st xs






-- second pass: generate all code to determine exact sizes with proper nested thread handling
secondPassGeneration :: GlobalSymbolTable->LocalVarStack -> [Stmt] -> (GlobalThreadsTable, [Instruction], Int)
secondPassGeneration gt st stmts = 
    do 
    let headerSize = calculateHeaderSize
    let (threads, mainBody, _) = collectAndGenerateThreads gt st stmts headerSize 0


        -- adjust thread start addresses by adding header size
        -- we could not know actual start address as we cannot generate 
        -- header before actually iterating threads and generate their bodies
        -- to dis[EndProg]++cover their sizes, 
        -- as precedent thread size obviously influence following thread start address

    --let adjustedThreads = map (\(tid, addr, body) -> (tid, addr + headerSize, body)) threads
    (threads, mainBody++[EndProg], headerSize)


collectAndGenerateThreads :: GlobalSymbolTable->LocalVarStack -> [Stmt] -> Int -> Int -> (GlobalThreadsTable, [Instruction], Int)

-- Base case: no more statements to process.
collectAndGenerateThreads _ _ [] la threadCounter = do 
    -- at the end of its execution the thread must decrement the global join counter
    -- so main thread can wait for every others
    let joinCode = [ TestAndSet (DirAddr joinLockAddr)       -- acquire lock for the join counter
                  , Receive r1
                   , Branch r1 (Rel (-2))                      -- if 1, lock was taken, so spin
                    -- if 0 , we got the lock, so proceed                   
                   , Load (DirAddr threadJoinAddr) r1       -- load the join counter value
                   , Compute Decr r1 r1 r1                  -- decrement it
                   , WriteInstr r1 (DirAddr threadJoinAddr) -- write the new value back
                   , WriteInstr reg0 (DirAddr joinLockAddr) -- release the lock
                   ]

    -- don't add join counter decrementation to the main thread
    if threadCounter > 0
      then ([],[] , la)
      else
        ([],[],la)

collectAndGenerateThreads gt st (ThreadCreate body : rest) la threadCounter = 
    do 
    let threadId = threadCounter + 1
        

    -- push and fill a new level on local variables display stack
    -- stack is brand new as thread will run on separate processor !
    let st2 = fillLocalDisplayForThisBody [] body


        
    let joinLockMechanismSize = 7

    -- /!\ Don't be fooled ! When two nested threads are created
    -- they will have THE SAME MEMORY ADDRESSES for local variables, 
    -- as the local memory is not shared between 
    -- THIS IS COMPLETELY NORMAL
    -- (I lost my mind trying to fix a bug that was ultimately my own brain logic skill issue)
    
  
    -- RECURSIVELY collect nested threads from the thread body
    -- don't forget to add join counter decrement mechanism size
    -- so the calculated parent thread body length for NESTED threads 
    -- is the right one (i.e not having only the "thread logic" length as start address offset)
    let (nestedThreads, nestedBodies, _) = collectAndGenerateThreads gt [] body (la+1+joinLockMechanismSize) threadId
        
    -- generate the actual code for the thread's body


    -- at the end of its execution the thread must decrement the global join counter
    -- so main thread can wait for every others
    let joinCode = [ TestAndSet (DirAddr joinLockAddr)       -- acquire lock for the join counter
                  , Receive r1
                   , Branch r1 (Rel (-2))                      -- if 1, lock was taken, so spin
                    -- if 0 , we got the lock, so proceed                   
                   , Load (DirAddr threadJoinAddr) r1       -- load the join counter value
                   , Compute Decr r1 r1 r1                  -- decrement it
                   , WriteInstr r1 (DirAddr threadJoinAddr) -- write the new value back
                   , WriteInstr reg0 (DirAddr joinLockAddr) -- release the lock
                   ]
    
    let threadBodyCode = if threadId > 1
        -- add last join instruction sequence when we hit last nested body
        -- as no deeper one can put it
        then if (countNestedThreads body) == 0
              then
                joinCode++[EndProg]++nestedBodies++joinCode++[EndProg]  --generateThreadBodyWithNested gt st2 body nestedThreads 
              else 
                joinCode++[EndProg]++nestedBodies
        else nestedBodies  --generateThreadBodyWithNested gt st2 body nestedThreads 
    -- let threadBodyCode = ownCode++[EndProg]++nestedBodies

      
    let threadSize = length threadBodyCode



    -- the main thread must insert a jump to skip over the thread body
    let jumpInstruction = if null rest 
                          then []
                          else [Jump (Rel (threadSize+1))]

    let jumpSize = length jumpInstruction -- always 1 but we might change the jump so we never know

    let maxNestedId = if null nestedThreads 
                         then threadId 
                         else maximum (map (\(tid, _, _) -> tid) nestedThreads)
      
    let startSeqSize = 2

    -- the code for the rest of the program starts after our jump and the thread's body
    let (restThreads, restCode, finalAddr) = collectAndGenerateThreads gt st rest (la + jumpSize+startSeqSize + threadSize+1) maxNestedId
    

    -- new thread's start address is immediately after the jump instruction
    let thisThread = (threadId, la + jumpSize+startSeqSize+1 , body)

    -- signal previous thread to start this one
    let startSequence = generateStartSequence thisThread

    -- thread table
    let allThreads = thisThread : nestedThreads ++ restThreads
        
    -- final code layout: the main thread's jump, the thread's body, and then the rest of the main code
    let finalCode = startSequence ++ jumpInstruction ++ threadBodyCode ++ restCode
        
    (allThreads, finalCode, finalAddr)
    
-- default case for other statements
collectAndGenerateThreads gt st (stmt : rest) currentAddr threadCounter = 
    do 
    let stmtCode = generateStmtCode gt st stmt
    let stmtSize = length stmtCode
    let (restThreads, restCode, finalAddr) = collectAndGenerateThreads gt st rest (currentAddr + stmtSize) threadCounter
    
    -- return thread table generated by following statements
    -- current generated code and last address used
    (restThreads, stmtCode ++ restCode, finalAddr)



generateThreadBodyWithNested :: GlobalSymbolTable->LocalVarStack-> [Stmt] -> GlobalThreadsTable -> [Instruction]
generateThreadBodyWithNested gt st body nestedThreads = 
    do 
    -- Generate the thread's own code by mapping over its statements.
    -- The `generateStmtCodeForThread` correctly skips nested `ThreadCreate` statements,
    -- as their bodies are laid out by the main `collectAndGenerateThreads` function.
    let ownCode = concatMap (generateStmtCodeForThread gt st nestedThreads 0) body

    -- at the end of its execution the thread must decrement the global join counter
    -- so main thread can wait for every others
    let joinCode = [ TestAndSet (DirAddr joinLockAddr)       -- acquire lock for the join counter
                  , Receive r1
                   , Branch r1 (Rel (-2))                      -- if 1, lock was taken, so spin
                    -- if 0 , we got the lock, so proceed                   
                   , Load (DirAddr threadJoinAddr) r1       -- load the join counter value
                   , Compute Decr r1 r1 r1                  -- decrement it
                   , WriteInstr r1 (DirAddr threadJoinAddr) -- write the new value back
                   , WriteInstr reg0 (DirAddr joinLockAddr) -- release the lock
                   ]
    

    let nestedBodies = concatMap (\(_, _, nestedBody) -> 
                                  do
                                    -- push and fill a new level on local variables display stack
                                    let st2 = fillLocalDisplayForThisBody [] nestedBody 
                                    generateThreadBodyWithNested gt st2 nestedBody []) 
                            
                                nestedThreads

    ownCode ++ joinCode ++ [EndProg]++nestedBodies -- don't forget to terminate thread


generateStartSequence :: ThreadInfo->[Instruction]
generateStartSequence (tid,sa,body) = [ 
  Load (ImmValue sa) regC
  ,WriteInstr regC (DirAddr (globalVarStartAddr+tid))
  ]

-- like generateThreadBodyWithNested but for normal bodies e.g if body
-- putting threads in if/else and while is not supported
-- so we just generate the classic body
generateNormalBody :: GlobalSymbolTable->LocalVarStack -> [Stmt] -> [Instruction]
generateNormalBody gt st = concatMap (generateStmtCode gt st)



-- generate statement code within a thread context (handles nested ThreadCreate)
generateStmtCodeForThread :: GlobalSymbolTable->LocalVarStack -> GlobalThreadsTable -> Int -> Stmt -> [Instruction]
generateStmtCodeForThread gt _ nestedThreads la (ThreadCreate body) = 
    -- when we encounter a ThreadCreate inside a thread, it should be handled
    -- by the nested thread system, so we don't generate code here
    []
generateStmtCodeForThread gt st nestedThreads la stmt = 
    generateStmtCode gt st stmt


countNestedThreads :: [Stmt]->Int
countNestedThreads [] = 0
countNestedThreads ((ThreadCreate _):ts) = 1+ (countNestedThreads ts)
countNestedThreads (_:ts) = countNestedThreads ts


-- calculate header size based on number of threads
calculateHeaderSize :: Int
calculateHeaderSize = do
    -- let numThreads = length threads
    --let setupSize = numThreads * 2  -- 2 instructions per thread setup
    let jumpLogicSize = 8  -- fixed size for jump logic
    let branchSize = 1     -- initial branch instruction
    let joinCounter = 2
    branchSize + jumpLogicSize + joinCounter

generateThreadJumpCode :: GlobalThreadsTable->[Instruction]
generateThreadJumpCode _ = [
        -- writeInstr WILL GO THERE
          
         Jump (Rel 8)               -- sprockell 0 jumps to skip thread repartition
         -- beginLoop
         -- store jump addr in shared memory findable by the target thread
         -- offset added to not overwrite already present join lock and counter
         ,Load (ImmValue globalVarStartAddr) r1 
         ,Compute Sprockell.Add r1 regSprID r1 
         , ReadInstr (IndAddr r1)

         , Receive r1
         , Compute Equal r1 reg0 r2
         , Branch r2 (Rel (-3))
         -- , WriteInstr regA numberIO
         , Jump (Ind r1)

        -- REST OF THE PROGRAM WILL GO THERE

       ]

-- (name, threadId, startAddress, body)
--generateThreadJumpCode ((id,sa,body):ts) = [Load (ImmValue sa) regC,WriteInstr regC (DirAddr (globalVarStartAddr+id))]
--  ++ generateThreadJumpCode ts


-- put some instructions in header before building the thread dispatcher 
-- (branch + join counter init)
buildHeader :: GlobalThreadsTable->[Instruction]
-- length tt*2 corresponds to loads + write, 
-- +2 corresponds to jump + actual target instruction
-- +2 for join counter initialisation
buildHeader tt = [Branch regSprID (Rel (length tt*2+2+2)),
  Load (ImmValue (length tt)) r1 , 
  WriteInstr r1 (DirAddr threadJoinAddr)]
  ++ generateThreadJumpCode tt



-- generate code for a statement
generateStmtCode :: GlobalSymbolTable->LocalVarStack-> Stmt -> [Instruction]
generateStmtCode gt st (Declaration typ name) = 
  case typ of
    -- variable declaration is already taken care of, 
    -- we just add initialisation to 0 for locals
    (Global Lock) -> [] -- locks are already taken care of
    (Global _) -> []
    _ -> [Load (ImmValue 0) r1,Store r1 (DirAddr $ getMemAddrforLocalVar st name)]

generateStmtCode gt st (Assignment name expr) =
  do
  let exprCode = generateExprCode gt st expr

  -- TODO : differentiate the case of local vars from global ones
  -- (Store vs WriteInstr)
  -- priority is given on local vars in case of name collision

  let result = getMemAddrforLocalVarMaybe st name
  case result of
    -- name refers to a global variable
    Nothing -> do 
      let varAddr = getMemAddrFromTable gt name
      exprCode ++ [Pop r1,WriteInstr r1 (DirAddr varAddr)]

    -- local one
    (Just addr)-> exprCode ++ [Pop r1,Store r1 (DirAddr addr)]

  


generateStmtCode gt st (If cond body1 body2) =
  do
    let st1 = fillLocalDisplayForThisBody (newBlockInStack st) body1
    let st2 = fillLocalDisplayForThisBody (newBlockInStack st) body2
    let condCode = generateExprCode gt st cond
    let elseBodyCode = generateNormalBody gt st2 body2
    let ifBodyCode = generateNormalBody gt st1 body1
        -- we use NOP as a fallback for condition as we don't know anything about what's after
    condCode ++ [Pop r1] ++ [Branch r1 (Rel (length elseBodyCode + 2))] -- Jump to If 
      ++ elseBodyCode
      ++[Jump (Rel (length ifBodyCode +1))] -- jump to NOP 
      ++ ifBodyCode 
      ++ [Nop] -- fallback for end of condition


generateStmtCode gt st (While cond body) =
  do 
    let stw = fillLocalDisplayForThisBody (newBlockInStack st) body
    let condCode = generateExprCode gt st cond
    let bodyCode = generateNormalBody gt stw body
    let loopStart = length bodyCode + length condCode + 4
    let loopEnd = length bodyCode + length condCode + 3

  -- same here as for condition, NOP as fallback after while
    --[Jump (Rel loopStart)] 
    condCode 
      ++ [Pop r1]
      ++ [Branch r1 (Rel 2), Jump (Rel (length bodyCode + 2))] 
      ++ bodyCode ++ [Jump (Rel (-loopEnd))] 
      ++ [Nop]

    




-- print array of numbers
-- generateStmtCode globalTable tt _ (Print (ArrayLit)) =
--   let exprCode = generateExprCode globalTable e
--   in exprCode ++ [WriteInstr r1 charIO]

-- print number
generateStmtCode gt st (Print e) =
  let exprCode = generateExprCode gt st e
  -- writeString do not uses stack, so Pop r1 still gives us the expression value
  --in exprCode ++writeString "OUT : " ++ [Pop r1,WriteInstr r1 numberIO]
  in exprCode ++ [Pop r1,WriteInstr r1 numberIO]

generateStmtCode _ _ (ThreadJoin) = [
          ReadInstr (DirAddr threadJoinAddr), 
          Receive r1,
          Branch r1 (Rel 2), 
          Jump (Rel 2), -- if 0, pass
          Jump (Rel (-4)) -- if not 0, loop back
          ]

generateStmtCode gt _ (LockFree lockName) =
  let lockAddr = getMemAddrFromTable gt lockName
  in [WriteInstr reg0 (DirAddr lockAddr)] -- release lock by writing 0


generateStmtCode gt _ (LockGet lockName) =
  let lockAddr = getMemAddrFromTable gt lockName
  -- acquire lock with test-and-set
  in [TestAndSet (DirAddr lockAddr), Receive r1,Branch r1 (Rel 2),Jump (Rel (-3))] 


generateStmtCode gt st (ScopeBlock body) = generateNormalBody gt st body






-- generate code for an expression
generateExprCode :: GlobalSymbolTable->LocalVarStack-> Expr -> [Instruction]

generateExprCode _ _ (IntLit n) = [Load (ImmValue (fromIntegral n)) r1, Push r1]
-- store booleans as int in {0,1}
generateExprCode _ _ (BoolLit b) = [Load (ImmValue (if b then 1 else 0)) r1, Push r1]

generateExprCode gt st (Var name) = do

    let result = getMemAddrforLocalVarMaybe st name
    case result of

      -- name refers to a global variable
      Nothing -> do 
        let varAddr = getMemAddrFromTable gt name
        [Receive r1,Push r1]

      -- local one
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
    ++ e2Code
    ++ [Pop r2]  
    ++ [Pop r1]
    -- assume e1 is in r1, e2 in r2, result in r3
    ++ [Compute opCode r1 r2 r3]
    -- put result in stack
    ++ [Push r3] 



generateExprCode gt st (UnOp op e) =
  do
  let eCode = generateExprCode gt st e
  let computeCode = 
        case op of
          -- booleans are ints in {0,1} so we need to make ifs
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

  -- index of expression is also offset in array, as we assume one address point on 4 bytes ;)
  in concatMap (\ (idx, exprCode) -> exprCode ++ [Store r1 (DirAddr (arrayAddr + idx))]) (zip [0..] exprCodes)

generateExprCode gt st (ArrayAccess arrayName indexExpr) =
  let indexCode = generateExprCode gt st indexExpr
      arrayAddr = getMemAddrFromTable gt arrayName

  in indexCode ++ [Load (DirAddr (arrayAddr + r1)) r1,Push r1] -- load array element at address arrayAddr + index


-- CODE FROM GITHUB SPROCKELL EXAMPLES
-- see https://github.com/bobismijnnaam/sprockell/blob/master/demos/DemoCharIO.hs
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


-- register and memory address management
r1, r2, r3 :: RegAddr
r1 = regA
r2 = regB
r3 = regC

-- in global memory
threadJoinAddr :: MemAddr
threadJoinAddr = 0x0002 -- irrationnal fear of using address 1, sorry
-- in global memory
joinLockAddr :: MemAddr 
joinLockAddr = 0x0003
-- in global memory
lockStartAddr :: MemAddr
lockStartAddr = 0x0004

globalVarStartAddr :: MemAddr
globalVarStartAddr = 0x0005

-- in local memory
localVarStartAddr :: MemAddr
localVarStartAddr = 0x0001


codeGen :: [Stmt] -> [Instruction]
codeGen ss = do
  let (gt,st) = (firstPassGeneration ss 0)
  let (tt,body,la) = secondPassGeneration gt st ss
  let header = buildHeader tt
  header ++ body


-- TODO : tests 

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



-- move wrinting specific start addresses before thread declaration
-- 
-- reset join counter before every thread region
