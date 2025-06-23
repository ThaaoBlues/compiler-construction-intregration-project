module MyCodeGen
    ( codeGen ) where

import Sprockell (Instruction(..), RegAddr, MemAddr, AddrImmDI(..), Target(..), SprID,Operator(..), reg0, RegAddr)
import MyParser (Stmt(..), Expr(..), Op(..), Type(..))

-- Global symbol table type
type GlobalSymbolTable = [(String, MemAddr)]

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

-- Generate code for a list of statements
generateCode :: GlobalSymbolTable -> [Stmt] -> [Instruction]
generateCode globalTable stmts = concatMap (generateStmtCode globalTable) stmts

-- Generate code for a statement
generateStmtCode :: GlobalSymbolTable -> Stmt -> [Instruction]
generateStmtCode globalTable (Declaration typ name) =
  case typ of
    (Global _) -> let (newGlobalTable, addr) = addGlobalVariable name typ globalTable
                  in []
    _ -> [] -- For local variables, we need to manage registers


generateStmtCode globalTable (Assignment var expr) =
  let exprCode = generateExprCode globalTable expr
      varAddr = getMemAddrFromTable var globalTable
  in exprCode ++ [Store r1 (DirAddr varAddr)]


generateStmtCode globalTable (If cond body) =
  let condCode = generateExprCode globalTable cond
      bodyCode = generateCode globalTable body
      condReg = r1 -- Assume condition is in r1
      -- we use NOP as a fallback for condition as we don't know anything about what's after
  in condCode ++ [Branch condReg (Rel (length bodyCode + 1))] ++ bodyCode ++ [Nop] -- Placeholder for end of condition


generateStmtCode globalTable (While cond body) =
  let condCode = generateExprCode globalTable cond
      bodyCode = generateCode globalTable body
      condReg = r1 -- Assume condition is in r1
      loopStart = length bodyCode + length condCode + 3
      loopEnd = length bodyCode + length condCode + 2

  -- same here as for condition, NOP as fallback after while
  in [Jump (Rel loopStart)] ++ condCode ++ [Branch condReg (Rel (length bodyCode + 1)), Jump (Rel (length bodyCode + 2))] ++ bodyCode ++ [Jump (Rel (-loopStart))] ++ [Nop]


generateStmtCode globalTable (Print expr) =
  let exprCode = generateExprCode globalTable expr
  in exprCode ++ [WriteInstr r1 (DirAddr outputAddress)]


generateStmtCode globalTable (ThreadCreate body) =
  let bodyCode = generateCode globalTable body
  in bodyCode ++ [EndProg]


-- IDEA : each thread sends a 1 value when exiting
-- Thus we can copy the loop used in the demo to wait for end of thread
generateStmtCode globalTable (ThreadJoin) = [] -- To implement: synchronization to wait for threads

generateStmtCode globalTable (StartThread threadName) = [] -- To implement: start a specific thread

generateStmtCode globalTable (LockCreate lockName) =
  let (newGlobalTable, addr) = addLock lockName globalTable
  in []


generateStmtCode globalTable (LockFree lockName) =
  let lockAddr = getMemAddrFromTable lockName globalTable
  in [WriteInstr 0 (DirAddr lockAddr)] -- Release lock by writing 0


generateStmtCode globalTable (LockGet lockName) =
  let lockAddr = getMemAddrFromTable lockName globalTable
  in [TestAndSet (DirAddr lockAddr), Receive r1] -- Acquire lock with test-and-set


generateStmtCode globalTable (ScopeBlock body) = generateCode globalTable body

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
                MyParser.Not -> [Compute ] 
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
r1 = 0
r2 = 1
r3 = 2

tempAddr1 :: MemAddr
tempAddr1 = 0xFFFE -- Temporary address for storing intermediate values

outputAddress :: MemAddr
outputAddress = 0xFFFF -- Fixed address for output operations

-- Generate a full program with multiple Sprockells
generateFullProgram :: GlobalSymbolTable -> [Stmt] -> [[Instruction]]
generateFullProgram = generateCode

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


-- TODO : Booleans inversion, thread execution,thread join, local variables (register constraints), tests 