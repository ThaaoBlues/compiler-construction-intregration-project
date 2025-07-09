import MyParser
import Test.Hspec
import Test.QuickCheck
import Data.Either
import MyCodeGen (codeGen,firstPassGeneration)
import Sprockell (DbgInput,localMem,sharedMem,sprStates,Instruction(..), RegAddr, MemAddr, AddrImmDI(..), Target(..), SprID,Operator(..), reg0, RegAddr, regA, regB, regC, regSprID, charIO, run, runWithDebugger, debuggerSimplePrint)
import GHC.Conc.Sync (sharedCAF)
import MyParser (fillGlobalSymbolTable)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
-- Helper functions for type checker tests
checkTypeValid :: [Stmt] -> Expectation
checkTypeValid stmts = stackChecking stmts (MyParser.fillGlobalSymbolTable stmts) [fillSymbolTable stmts] `shouldBe` []

checkTypeInvalid :: [Stmt] -> Expectation
checkTypeInvalid stmts = do 
    let errors =stackChecking stmts (MyParser.fillGlobalSymbolTable stmts) [fillSymbolTable stmts]
    print "errors detected by compiler : \n"
    print errors
    errors `shouldNotBe` []

-- tests for the parser
parserSpec :: Spec
parserSpec = describe "Parser" $ do
    it "parses a valid integer declaration" $ do
        let input = "entero x :)"
        parseMyLang input `shouldBe` Right [Declaration Entero "x"]

    it "rejects an invalid integer declaration" $ do
        let input = "entero x"  -- missing ":)"
        parseMyLang input `shouldSatisfy` isLeft

    it "rejects an invalid integer declaration" $ do
        let input = "enterox:)"  -- missing " "
        parseMyLang input `shouldSatisfy` isLeft

    it "parses a valid boolean declaration" $ do
        let input = "booleana yes:)"
        parseMyLang input `shouldBe` Right [Declaration Booleana "yes"]

    it "rejects an invalid boolean declaration" $ do
        let input = "booleana x"  -- missing ":)"
        parseMyLang input `shouldSatisfy` isLeft

    it "rejects an invalid boolean declaration" $ do
        let input = "booleana y = verdad:)"  -- cannot have assignment in declaration
        parseMyLang input `shouldSatisfy` isLeft

    it "parses a valid array declaration" $ do
        let input = "array entero a:)"
        parseMyLang input `shouldBe` Right [Declaration (Array Entero) "a"]

    it "rejects an invalid array declaration" $ do
        let input = "array a:)"  -- missing type
        parseMyLang input `shouldSatisfy` isLeft

    it "rejects an invalid array declaration" $ do
        let input = "array booleana:)"  -- missing identifier
        parseMyLang input `shouldSatisfy` isLeft

    it "parses a valid integer assignment" $ do
        let input = "x = 5 :)"
        parseMyLang input `shouldBe` Right [Assignment "x" (IntLit 5)]

    it "parses a valid boolean assignment" $ do
        let input = "x = verdad :)"
        parseMyLang input `shouldBe` Right [Assignment "x" (BoolLit True)]

    it "parses a valid boolean array assignment" $ do
        let input = "x = [verdad, mentira, verdad] :)"
        parseMyLang input `shouldBe` Right [Assignment "x" (ArrayLit [BoolLit True,BoolLit False,BoolLit True])]

    it "parses a valid integer array assignment" $ do
        let input = "x = [1, 2, 3] :)"
        parseMyLang input `shouldBe` Right [Assignment "x" (ArrayLit [IntLit 1,IntLit 2,IntLit 3])]

    it "parses an invalid integer array assignment" $ do
        let input = "x = 1,2,3 :)" -- missing "[]"
        parseMyLang input `shouldSatisfy` isLeft
    
    it "parses a valid array access" $ do
        let input = "array entero a:) a = [1,2,3]:) entero x:) x = a[1]:)"
        parseMyLang input `shouldBe` Right [Declaration (Array Entero) "a",Assignment "a" (ArrayLit [IntLit 1,IntLit 2,IntLit 3]),Declaration Entero "x",Assignment "x" (ArrayAccess "a" (IntLit 1))]

    it "parses a valid integer addition" $ do
        let input = "x = 1 + 5 :)"
        parseMyLang input `shouldBe` Right [Assignment "x" (BinOp MyParser.Add (IntLit 1) (IntLit 5))]

    it "parses a valid integer subtraction" $ do
        let input = "x = 1 - 5 :)"
        parseMyLang input `shouldBe` Right [Assignment "x" (BinOp MyParser.Sub (IntLit 1) (IntLit 5))]

    it "parses a valid integer multiplication" $ do
        let input = "x = 1 * 5 :)"
        parseMyLang input `shouldBe` Right [Assignment "x" (BinOp MyParser.Mul (IntLit 1) (IntLit 5))]

    it "parses a valid integer equality" $ do
        let input = "eq = x == 5 :)"
        parseMyLang input `shouldBe` Right [Assignment "eq" (BinOp Eq (Var "x") (IntLit 5))]

    it "parses a valid integer equality" $ do
        let input = "eq = x == 5 :)"
        parseMyLang input `shouldBe` Right [Assignment "eq" (BinOp Eq (Var "x") (IntLit 5))]

    it "parses a valid integer inequality" $ do
        let input = "eq = x != 5 :)"
        parseMyLang input `shouldBe` Right [Assignment "eq" (BinOp Neq (Var "x") (IntLit 5))]

    it "parses a valid integer greater then" $ do
        let input = "eq = x > 5 :)"
        parseMyLang input `shouldBe` Right [Assignment "eq" (BinOp MyParser.Gt (Var "x") (IntLit 5))]

    it "parses a valid integer greater equal" $ do
        let input = "eq = x >= 5 :)"
        parseMyLang input `shouldBe` Right [Assignment "eq" (BinOp Geq (Var "x") (IntLit 5))]

    it "parses a valid integer smaller then" $ do
        let input = "eq = x < 5 :)"
        parseMyLang input `shouldBe` Right [Assignment "eq" (BinOp MyParser.Lt (Var "x") (IntLit 5))]

    it "parses a valid integer smaller equal" $ do
        let input = "eq = x <= 5 :)"
        parseMyLang input `shouldBe` Right [Assignment "eq" (BinOp Leq (Var "x") (IntLit 5))]

    it "reject an invalid integer addition" $ do
        let input = "x + 5 :)" -- missing assignment (result needs to be assigned to a variable)
        parseMyLang input `shouldSatisfy` isLeft

    it "parses a valid boolean conjunction" $ do
        let input = "and = x Y y :)"
        parseMyLang input `shouldBe` Right [Assignment "and" (BinOp MyParser.And (Var "x") (Var "y"))]

    it "parses a valid boolean disjunction" $ do
        let input = "or = x O y :)"
        parseMyLang input `shouldBe` Right [Assignment "or" (BinOp MyParser.Or (Var "x") (Var "y"))]

    it "parses a valid boolean negation" $ do
        let input = "not = ~:(x :)"
        parseMyLang input `shouldBe` Right [Assignment "not" (UnOp Not (Var "x"))]

    it "parses a valid array equality" $ do
        let input = "eq = [1,2,3] == [4,5,6]:)"
        parseMyLang input `shouldBe` Right [Assignment "eq" (BinOp Eq (ArrayLit [IntLit 1,IntLit 2,IntLit 3]) (ArrayLit [IntLit 4,IntLit 5,IntLit 6]))]

    it "parses a valid scope declaration" $ do
        let input = "{entero x:)}"
        parseMyLang input `shouldBe` Right [ScopeBlock [Declaration Entero "x"]]

    it "parses a valid nested scope declaration" $ do
        let input = "{entero x:) {booleana a:)}}"
        parseMyLang input `shouldBe` Right [ScopeBlock [Declaration Entero "x",ScopeBlock [Declaration Booleana "a"]]]

    it "parses a valid print statement" $ do
        let input = "imprimir ¡x!:)"
        parseMyLang input `shouldBe` Right [Print (Var "x")]

    it "parses an invalid print statement" $ do
        let input = "imprimir x:)"
        parseMyLang input `shouldSatisfy` isLeft

    it "parses a valid if else statement" $ do
        let input = "si verdad { imprimir ¡5! :) } sino { imprimir ¡10! :) }"
        parseMyLang input `shouldBe` Right [If (BoolLit True) [Print (IntLit 5)] [Print (IntLit 10)]]

    it "parses a valid if statement" $ do
        let input = "si verdad { imprimir ¡5! :) }"
        parseMyLang input `shouldBe` Right [If (BoolLit True) [Print (IntLit 5)] []]

    it "parses a valid while statement" $ do
        let input = "durante verdad {x = 3:)}"
        parseMyLang input `shouldBe` Right [While (BoolLit True) [Assignment "x" (IntLit 3)]]   

    it "rejects an invalid while statement" $ do
        let input = "durante verdad x = 3:)" -- missing "{}"
        parseMyLang input `shouldSatisfy` isLeft 

    it "rejects an invalid program" $ do
        let input = "x = 5"  -- missing ":)"
        parseMyLang input `shouldSatisfy` isLeft

    it "parses program with multiple line comment" $ do
        let input = "booleana yes :) /* hello \n */ \n yes = verdad:)"
        parseMyLang input `shouldBe` Right [Declaration Booleana "yes",Assignment "yes" (BoolLit True)]

    it "parses program with single line comment" $ do
        let input = "booleana yes:) //hello \n \n yes = verdad:)"
        parseMyLang input `shouldBe` Right [Declaration Booleana "yes",Assignment "yes" (BoolLit True)]

    it "rejects program with incorrect multi line comment" $ do
        let input = "booleana yes:) /*hello \n \n yes = verdad:)" -- missing "*/"
        parseMyLang input `shouldSatisfy` isLeft

    it "parses a valid thread creation" $ do
        let input = "hilo {entero x:) \n x = 5 + 1:)}"
        parseMyLang input `shouldBe` Right [ThreadCreate [Declaration Entero "x",Assignment "x" (BinOp MyParser.Add (IntLit 5) (IntLit 1))]]

    it "rejects an invalid thread creation" $ do
        let input = "hilo :)" -- missing scope
        parseMyLang input `shouldSatisfy` isLeft

    it "parses a valid join" $ do
        let input = "esperamos:)"
        parseMyLang input `shouldBe` Right [ThreadJoin]

    it "parses a valid global variable declaration" $ do
        let input = "global entero x:)" -- declares an integer
        parseMyLang input `shouldBe` Right [Declaration (Global Entero) "x"]

    it "parses an invalid global variable declaration" $ do
        let input = "global x:)" -- missing type
        parseMyLang input `shouldSatisfy` isLeft

    it "parses an invalid global variable declaration" $ do
        let input = "global entero:)" -- missing variable identifier
        parseMyLang input `shouldSatisfy` isLeft

    it "parses a valid lock creation" $ do
        let input = "esclusa lock:)"
        parseMyLang input `shouldBe` Right [LockCreate "lock"]

    it "parses a valid lock unlock (free)" $ do
        let input = "liberar lock:)"
        parseMyLang input `shouldBe` Right [LockFree "lock"]

    it "parses a valid lock getting" $ do
        let input = "obtener lock:)"
        parseMyLang input `shouldBe` Right [LockGet "lock"]   

    it "rejects an invalid lock creation" $ do
        let input = "esclusa :)" -- Missing lock name
        parseMyLang input `shouldSatisfy` isLeft

-- Tests for the type checker
typeCheckerSpec :: Spec
typeCheckerSpec = describe "Type Checker" $ do
    describe "Valid Programs" $ do
        it "accepts a valid assignment" $ do
            let stmts = [
                    Declaration Entero "x",
                    Assignment "x" (IntLit 5)
                    ]
            checkTypeValid stmts

        it "accepts a valid condition" $ do
            let stmts = [
                    If (BoolLit True) [Print (IntLit 5)] [Print (IntLit 7)]
                    ]
            checkTypeValid stmts

        it "accepts a valid array" $ do
            let stmts = [
                    Declaration (Array Entero) "arr",
                    Assignment "arr" (ArrayLit [IntLit 1, IntLit 2, IntLit 3])
                    ]
            checkTypeValid stmts

        it "accepts a valid while loop" $ do
            let stmts = [
                    While (BoolLit True) [Print (IntLit 5)]
                    ]
            checkTypeValid stmts

        it "accepts a valid thread creation" $ do
            let stmts = [
                    ThreadCreate [Print (IntLit 5)]
                    ]
            checkTypeValid stmts

        it "accepts a valid global declaration" $ do
            let stmts = [
                    (Declaration (Global Entero) "x")
                    ]
            checkTypeValid stmts

        it "accepts a valid lock creation" $ do
            let stmts = [
                    LockCreate "lock"
                    ]
            checkTypeValid stmts

        it "accepts a valid lock free" $ do
            let stmts = [
                    LockCreate "lock",
                    LockFree "lock"
                    ]
            checkTypeValid stmts

        it "accepts a valid lock get" $ do
            let stmts = [
                    LockCreate "lock",
                    LockGet "lock"
                    ]
            checkTypeValid stmts

    describe "Invalid Programs" $ do
        it "rejects an invalid assignment" $ do
            let stmts = [
                    Declaration Entero "x",
                    Assignment "x" (BoolLit True)  -- type error
                    ]
            checkTypeInvalid stmts

        it "rejects an invalid condition" $ do
            let stmts = [
                    If (IntLit 5) [Print (IntLit 5)] [Print (IntLit 7)]  -- non-boolean condition
                    ]
            checkTypeInvalid stmts

        it "rejects an undefined variable" $ do
            let stmts = [
                    Assignment "x" (IntLit 5)  -- undeclared variable
                    ]
            checkTypeInvalid stmts

        it "rejects an invalid array" $ do
            let stmts = [
                    Declaration (Array Entero) "arr",
                    Assignment "arr" (ArrayLit [IntLit 1, BoolLit True, IntLit 3])  -- non-homogeneous types
                    ]
            checkTypeInvalid stmts




        it "rejects an invalid while loop condition" $ do
            let stmts = [
                    While (IntLit 5) [Print (IntLit 5)]  -- non-boolean condition
                    ]
            checkTypeInvalid stmts

        it "rejects an invalid thread creation" $ do
            let stmts = [
                    ThreadCreate [Assignment "x" (IntLit 5)]  -- undeclared variable
                    ]
            checkTypeInvalid stmts

        it "rejects an invalid global declaration" $ do
            let stmts = [
                    (Declaration (Global Entero) "x"),
                    Assignment "x" (BoolLit True)  -- type error
                    ]
            checkTypeInvalid stmts

        -- locks are handled like global variables
        -- so if type checker pass this, it also handles classic global variables
        it "rejects an invalid lock free" $ do
            let stmts = [
                    LockCreate "lock",
                    LockFree "lock2"  -- undeclared lock
                    ]
            checkTypeInvalid stmts

        it "rejects an invalid lock free" $ do
            let stmts = [
                    LockFree "lock"  -- undeclared lock
                    ]
            checkTypeInvalid stmts

        it "rejects an invalid lock get" $ do
            let stmts = [
                    LockGet "lock"  -- undeclared lock
                    ]
            
            checkTypeInvalid stmts


codeGenSpec :: Spec
codeGenSpec = describe "Code generation" $ do
    it "basic print program" $ do 
        let prog = "imprimir¡5!:) /* test de commentaire */"
        let ast = case parseMyLang prog of
                (Left _) -> error "Parse error"
                (Right tree) -> tree  

        codeGen ast `shouldBe` [Branch 1 (Rel 2),Jump (Rel 8),Load (ImmValue 4) 2,Compute Sprockell.Add 2 1 4,ReadInstr (IndAddr 4),Receive 6,Compute Equal 6 0 7,Branch 7 (Rel (-4)),Jump (Ind 6),Load (ImmValue 5) 2,Push 2,Pop 2,WriteInstr 2 (DirAddr 65536),EndProg]
        
    it "basic if/else program" $ do 
        let prog = "entero x:) x=168:) si verdad { imprimir ¡x! :) } sino { imprimir ¡10! :) }"
        let ast = case parseMyLang prog of
                (Left s) -> error (show s)
                (Right tree) -> tree  

        codeGen ast `shouldBe` [Branch 1 (Rel 2),Jump (Rel 8),Load (ImmValue 4) 2,Compute Sprockell.Add 2 1 4,ReadInstr (IndAddr 4),Receive 6,Compute Equal 6 0 7,Branch 7 (Rel (-4)),Jump (Ind 6),Load (ImmValue 0) 2,Store 2 (DirAddr 1),Load (ImmValue 168) 2,Push 2,Pop 2,Store 2 (DirAddr 1),Load (ImmValue 1) 2,Push 2,Pop 2,Branch 2 (Rel 6),Load (ImmValue 10) 2,Push 2,Pop 2,WriteInstr 2 (DirAddr 65536),Jump (Rel 5),Load (DirAddr 1) 2,Push 2,Pop 2,WriteInstr 2 (DirAddr 65536),Nop,EndProg]

    it "basic thread creation" $ do 
        let prog = "entero x:) hilo { entero x:)}"
        
        let ast = case parseMyLang prog of
                (Left e) -> error (show e)
                (Right tree) -> tree

        codeGen ast `shouldBe` [Branch 1 (Rel 2),Jump (Rel 8),Load (ImmValue 4) 2,Compute Sprockell.Add 2 1 4,ReadInstr (IndAddr 4),Receive 6,Compute Equal 6 0 7,Branch 7 (Rel (-4)),Jump (Ind 6),Load (ImmValue 0) 2,Store 2 (DirAddr 1),TestAndSet (DirAddr 1),Receive 2,Branch 2 (Rel 2),Jump (Rel (-3)),ReadInstr (DirAddr 0),Receive 2,Compute Incr 2 2 2,WriteInstr 2 (DirAddr 0),WriteInstr 0 (DirAddr 1),Load (ImmValue 23) 4,WriteInstr 4 (DirAddr 5),Jump (Rel 13),Load (ImmValue 0) 2,Store 2 (DirAddr 1),TestAndSet (DirAddr 1),Receive 2,Branch 2 (Rel 2),Jump (Rel (-3)),ReadInstr (DirAddr 0),Receive 2,Compute Decr 2 2 2,WriteInstr 2 (DirAddr 0),WriteInstr 0 (DirAddr 1),EndProg,EndProg]    
    
    it "multiple non-nested thread creation" $ do 
        let prog = "entero x:) hilo { entero x:)} hilo { entero x:)} hilo { entero x:)}"
        
        let ast = case parseMyLang prog of
                (Left e) -> error (show e)
                (Right tree) -> tree

        codeGen ast `shouldBe` [Branch 1 (Rel 2),Jump (Rel 8),Load (ImmValue 4) 2,Compute Sprockell.Add 2 1 4,ReadInstr (IndAddr 4),Receive 6,Compute Equal 6 0 7,Branch 7 (Rel (-4)),Jump (Ind 6),Load (ImmValue 0) 2,Store 2 (DirAddr 1),TestAndSet (DirAddr 1),Receive 2,Branch 2 (Rel 2),Jump (Rel (-3)),ReadInstr (DirAddr 0),Receive 2,Compute Incr 2 2 2,WriteInstr 2 (DirAddr 0),WriteInstr 0 (DirAddr 1),Load (ImmValue 23) 4,WriteInstr 4 (DirAddr 5),Jump (Rel 13),Load (ImmValue 0) 2,Store 2 (DirAddr 1),TestAndSet (DirAddr 1),Receive 2,Branch 2 (Rel 2),Jump (Rel (-3)),ReadInstr (DirAddr 0),Receive 2,Compute Decr 2 2 2,WriteInstr 2 (DirAddr 0),WriteInstr 0 (DirAddr 1),EndProg,TestAndSet (DirAddr 1),Receive 2,Branch 2 (Rel 2),Jump (Rel (-3)),ReadInstr (DirAddr 0),Receive 2,Compute Incr 2 2 2,WriteInstr 2 (DirAddr 0),WriteInstr 0 (DirAddr 1),Load (ImmValue 47) 4,WriteInstr 4 (DirAddr 6),Jump (Rel 13),Load (ImmValue 0) 2,Store 2 (DirAddr 1),TestAndSet (DirAddr 1),Receive 2,Branch 2 (Rel 2),Jump (Rel (-3)),ReadInstr (DirAddr 0),Receive 2,Compute Decr 2 2 2,WriteInstr 2 (DirAddr 0),WriteInstr 0 (DirAddr 1),EndProg,TestAndSet (DirAddr 1),Receive 2,Branch 2 (Rel 2),Jump (Rel (-3)),ReadInstr (DirAddr 0),Receive 2,Compute Incr 2 2 2,WriteInstr 2 (DirAddr 0),WriteInstr 0 (DirAddr 1),Load (ImmValue 71) 4,WriteInstr 4 (DirAddr 7),Jump (Rel 13),Load (ImmValue 0) 2,Store 2 (DirAddr 1),TestAndSet (DirAddr 1),Receive 2,Branch 2 (Rel 2),Jump (Rel (-3)),ReadInstr (DirAddr 0),Receive 2,Compute Decr 2 2 2,WriteInstr 2 (DirAddr 0),WriteInstr 0 (DirAddr 1),EndProg,EndProg]
    

    it "2 nested thread creation" $ do 
        let prog = "entero x:) hilo { entero x:) hilo {entero x:)}}"
        
        let ast = case parseMyLang prog of
                (Left e) -> error (show e)
                (Right tree) -> tree

        codeGen ast `shouldBe` [Branch 1 (Rel 2),Jump (Rel 8),Load (ImmValue 4) 2,Compute Sprockell.Add 2 1 4,ReadInstr (IndAddr 4),Receive 6,Compute Equal 6 0 7,Branch 7 (Rel (-4)),Jump (Ind 6),Load (ImmValue 0) 2,Store 2 (DirAddr 1),TestAndSet (DirAddr 1),Receive 2,Branch 2 (Rel 2),Jump (Rel (-3)),ReadInstr (DirAddr 0),Receive 2,Compute Incr 2 2 2,WriteInstr 2 (DirAddr 0),WriteInstr 0 (DirAddr 1),Load (ImmValue 23) 4,WriteInstr 4 (DirAddr 5),Jump (Rel 37),Load (ImmValue 0) 2,Store 2 (DirAddr 1),TestAndSet (DirAddr 1),Receive 2,Branch 2 (Rel 2),Jump (Rel (-3)),ReadInstr (DirAddr 0),Receive 2,Compute Incr 2 2 2,WriteInstr 2 (DirAddr 0),WriteInstr 0 (DirAddr 1),Load (ImmValue 37) 4,WriteInstr 4 (DirAddr 6),Jump (Rel 13),Load (ImmValue 0) 2,Store 2 (DirAddr 1),TestAndSet (DirAddr 1),Receive 2,Branch 2 (Rel 2),Jump (Rel (-3)),ReadInstr (DirAddr 0),Receive 2,Compute Decr 2 2 2,WriteInstr 2 (DirAddr 0),WriteInstr 0 (DirAddr 1),EndProg,TestAndSet (DirAddr 1),Receive 2,Branch 2 (Rel 2),Jump (Rel (-3)),ReadInstr (DirAddr 0),Receive 2,Compute Decr 2 2 2,WriteInstr 2 (DirAddr 0),WriteInstr 0 (DirAddr 1),EndProg,EndProg]

    it "multiple nested thread creation" $ do 
        let prog = "entero x:) hilo { entero x:) hilo { entero x:) hilo { entero x:) entero y:) hilo { entero x:) } } } }"
        
        let ast = case parseMyLang prog of
                (Left e) -> error (show e)
                (Right tree) -> tree

        codeGen ast `shouldBe` [Branch 1 (Rel 2),Jump (Rel 8),Load (ImmValue 4) 2,Compute Sprockell.Add 2 1 4,ReadInstr (IndAddr 4),Receive 6,Compute Equal 6 0 7,Branch 7 (Rel (-4)),Jump (Ind 6),Load (ImmValue 0) 2,Store 2 (DirAddr 1),TestAndSet (DirAddr 1),Receive 2,Branch 2 (Rel 2),Jump (Rel (-3)),ReadInstr (DirAddr 0),Receive 2,Compute Incr 2 2 2,WriteInstr 2 (DirAddr 0),WriteInstr 0 (DirAddr 1),Load (ImmValue 23) 4,WriteInstr 4 (DirAddr 5),Jump (Rel 87),Load (ImmValue 0) 2,Store 2 (DirAddr 1),TestAndSet (DirAddr 1),Receive 2,Branch 2 (Rel 2),Jump (Rel (-3)),ReadInstr (DirAddr 0),Receive 2,Compute Incr 2 2 2,WriteInstr 2 (DirAddr 0),WriteInstr 0 (DirAddr 1),Load (ImmValue 37) 4,WriteInstr 4 (DirAddr 6),Jump (Rel 63),Load (ImmValue 0) 2,Store 2 (DirAddr 1),TestAndSet (DirAddr 1),Receive 2,Branch 2 (Rel 2),Jump (Rel (-3)),ReadInstr (DirAddr 0),Receive 2,Compute Incr 2 2 2,WriteInstr 2 (DirAddr 0),WriteInstr 0 (DirAddr 1),Load (ImmValue 51) 4,WriteInstr 4 (DirAddr 7),Jump (Rel 39),Load (ImmValue 0) 2,Store 2 (DirAddr 2),Load (ImmValue 0) 2,Store 2 (DirAddr 1),TestAndSet (DirAddr 1),Receive 2,Branch 2 (Rel 2),Jump (Rel (-3)),ReadInstr (DirAddr 0),Receive 2,Compute Incr 2 2 2,WriteInstr 2 (DirAddr 0),WriteInstr 0 (DirAddr 1),Load (ImmValue 67) 4,WriteInstr 4 (DirAddr 8),Jump (Rel 13),Load (ImmValue 0) 2,Store 2 (DirAddr 1),TestAndSet (DirAddr 1),Receive 2,Branch 2 (Rel 2),Jump (Rel (-3)),ReadInstr (DirAddr 0),Receive 2,Compute Decr 2 2 2,WriteInstr 2 (DirAddr 0),WriteInstr 0 (DirAddr 1),EndProg,TestAndSet (DirAddr 1),Receive 2,Branch 2 (Rel 2),Jump (Rel (-3)),ReadInstr (DirAddr 0),Receive 2,Compute Decr 2 2 2,WriteInstr 2 (DirAddr 0),WriteInstr 0 (DirAddr 1),EndProg,TestAndSet (DirAddr 1),Receive 2,Branch 2 (Rel 2),Jump (Rel (-3)),ReadInstr (DirAddr 0),Receive 2,Compute Decr 2 2 2,WriteInstr 2 (DirAddr 0),WriteInstr 0 (DirAddr 1),EndProg,TestAndSet (DirAddr 1),Receive 2,Branch 2 (Rel 2),Jump (Rel (-3)),ReadInstr (DirAddr 0),Receive 2,Compute Decr 2 2 2,WriteInstr 2 (DirAddr 0),WriteInstr 0 (DirAddr 1),EndProg,EndProg]


    it "while loop with counter" $ do
            let prog = "entero x:) x = 0:) durante x < 3 { x = x + 1 :) }"
            let ast = case parseMyLang prog of
                    (Left _) -> error "Parse error"
                    (Right tree) -> tree
            codeGen ast `shouldBe` [Branch 1 (Rel 2),Jump (Rel 8),Load (ImmValue 4) 2,Compute Sprockell.Add 2 1 4,ReadInstr (IndAddr 4),Receive 6,Compute Equal 6 0 7,Branch 7 (Rel (-4)),Jump (Ind 6),Load (ImmValue 0) 2,Store 2 (DirAddr 1),Load (ImmValue 0) 2,Push 2,Pop 2,Store 2 (DirAddr 1),Load (DirAddr 1) 2,Push 2,Load (ImmValue 3) 2,Push 2,Pop 3,Pop 2,Compute Sprockell.Lt 2 3 4,Push 4,Pop 2,Branch 2 (Rel 2),Jump (Rel 12),Load (DirAddr 1) 2,Push 2,Load (ImmValue 1) 2,Push 2,Pop 3,Pop 2,Compute Sprockell.Add 2 3 4,Push 4,Pop 2,Store 2 (DirAddr 1),Jump (Rel (-21)),Nop,EndProg]


    it "lock operations" $ do
        let prog = unlines [
                "esclusa lock:)",
                "obtener lock:)",
                "imprimir ¡1!:)",
                "liberar lock:)"
                ]
        let ast = case parseMyLang prog of
                (Left _) -> error "Parse error"
                (Right tree) -> tree
        codeGen ast `shouldBe` [Branch 1 (Rel 2),Jump (Rel 8),Load (ImmValue 4) 2,Compute Sprockell.Add 2 1 4,ReadInstr (IndAddr 4),Receive 6,Compute Equal 6 0 7,Branch 7 (Rel (-4)),Jump (Ind 6),TestAndSet (DirAddr 2),Receive 2,Branch 2 (Rel 2),Jump (Rel (-3)),Load (ImmValue 1) 2,Push 2,Pop 2,WriteInstr 2 (DirAddr 65536),WriteInstr 0 (DirAddr 2),EndProg]




runtimeSpec :: Spec
runtimeSpec = describe "Runtime tests" $ do
    it "prints a number" $ do
        let program = "imprimir ¡5!:)"
        case parseMyLang program of
            Left err -> expectationFailure (show err)
            Right ast -> do
                let prog = codeGen ast
                putStrLn "Generated code:"
                print prog
                run [prog]
                pendingWith "Manual verification needed: should print 5"

    it "adds two numbers" $ do
        let program = "entero x :) x = 3 + 2 :) imprimir ¡x!:)"
        case parseMyLang program of
            Left err -> expectationFailure (show err)
            Right ast -> do
                let prog = codeGen ast
                putStrLn "Generated code:"
                print prog
                run [prog]
                pendingWith "Manual verification needed: should print 5"

    it "while loop that counts to 5" $ do
        let program = "entero x :) x = 0 :) durante x < 5 { x = x + 1 :) } imprimir ¡x!:)"
        case parseMyLang program of
            Left err -> expectationFailure (show err)
            Right ast -> do
                let prog = codeGen ast
                putStrLn "Generated code:"
                print prog
                run [prog]
                pendingWith "Manual verification needed: should print 5"
           
    it "days in February calculation" $ do
        let program = unlines [ "entero year :)"
                , "year = 2020 :)" -- should be 29 days
                , "entero days :)"
                , "booleana leap :)"
                , ""
                , "// compute year % 4 "
                , "entero remainder4 :)"
                , "remainder4 = year :)"
                , "durante remainder4 >= 4 {"
                , "    remainder4 = remainder4 - 4 :)"
                , "}"
                , ""
                , "// compute year % 100 "
                , "entero remainder100 :)"
                , "remainder100 = year :)"
                , "durante remainder100 >= 100 {"
                , "    remainder100 = remainder100 - 100 :)"
                , "}"
                , ""
                , "// Compute year % 400 "
                , "entero remainder400 :)"
                , "remainder400 = year :)"
                , "durante remainder400 >= 400 {"
                , "    remainder400 = remainder400 - 400 :)"
                , "}"
                , ""
                , "leap = ¡remainder4 == 0! Y ¡¡remainder100 != 0! O ¡remainder400 == 0!! :)"
                , "si leap { days = 29 :) } sino { days = 28 :) }"
                , "imprimir ¡days! :)"
                ]
        case parseMyLang program of
            Left err -> expectationFailure (show err)
            Right ast -> do
                let prog = codeGen ast
                putStrLn "Generated code:"
                print prog
                run [prog]
                pendingWith "Manual verification needed: should print 29"


    -- it "infinite loop" $ do
    --     let program = "durante verdad { }"  -- infinite loop
    --     case parseMyLang program of
    --         Left err -> expectationFailure (show err)
    --         Right ast -> do
    --             let prog = codeGen ast
    --             putStrLn "Generated code:"
    --             print prog
    --             run [prog]
    --             pendingWith "Manual verification needed: should run forever"

    -- TODO : add tests for while loops, locks and arrays in code generation
    -- Runtime errors :
    -- Add tests that contains runtime errors ( ex : div by 0) for each features
    -- Add multiple simple algorithms to calculate dumb stuff like number of days in a month of a year
    -- Add tests for non-terminating algorithms
    -- Add dumb programs that uses arrays.

extendedTest :: Spec
extendedTest = describe "Extended program" $ do
    it "Code generation" $ do
        setLocaleEncoding utf8
        input <- readFile "test.hola"
        let tree = case parseMyLang input of
                    Left err -> error (show err)
                    Right ast -> ast
        codeGen tree `shouldBe` [Branch 1 (Rel 2),Jump (Rel 8),Load (ImmValue 4) 2,Compute Sprockell.Add 2 1 4,ReadInstr (IndAddr 4),Receive 6,Compute Equal 6 0 7,Branch 7 (Rel (-4)),Jump (Ind 6),Load (ImmValue 1) 2,Push 2,Pop 2,WriteInstr 2 (DirAddr 3),Load (ImmValue 0) 2,Store 2 (DirAddr 2),Load (ImmValue 0) 2,Store 2 (DirAddr 1),Load (ImmValue 1) 2,Push 2,Pop 2,Store 2 (DirAddr 1),TestAndSet (DirAddr 1),Receive 2,Branch 2 (Rel 2),Jump (Rel (-3)),ReadInstr (DirAddr 0),Receive 2,Compute Incr 2 2 2,WriteInstr 2 (DirAddr 0),WriteInstr 0 (DirAddr 1),Load (ImmValue 33) 4,WriteInstr 4 (DirAddr 5),Jump (Rel 59),ReadInstr (DirAddr 3),Receive 2,Push 2,Pop 2,WriteInstr 2 (DirAddr 65536),TestAndSet (DirAddr 1),Receive 2,Branch 2 (Rel 2),Jump (Rel (-3)),ReadInstr (DirAddr 0),Receive 2,Compute Incr 2 2 2,WriteInstr 2 (DirAddr 0),WriteInstr 0 (DirAddr 1),Load (ImmValue 50) 4,WriteInstr 4 (DirAddr 6),Jump (Rel 32),TestAndSet (DirAddr 2),Receive 2,Branch 2 (Rel 2),Jump (Rel (-3)),ReadInstr (DirAddr 3),Receive 2,Push 2,Load (ImmValue 1) 2,Push 2,Pop 3,Pop 2,Compute Sprockell.Add 2 3 4,Push 4,Pop 2,WriteInstr 2 (DirAddr 3),ReadInstr (DirAddr 3),Receive 2,Push 2,Pop 2,WriteInstr 2 (DirAddr 65536),WriteInstr 0 (DirAddr 2),TestAndSet (DirAddr 1),Receive 2,Branch 2 (Rel 2),Jump (Rel (-3)),ReadInstr (DirAddr 0),Receive 2,Compute Decr 2 2 2,WriteInstr 2 (DirAddr 0),WriteInstr 0 (DirAddr 1),EndProg,TestAndSet (DirAddr 1),Receive 2,Branch 2 (Rel 2),Jump (Rel (-3)),ReadInstr (DirAddr 0),Receive 2,Compute Decr 2 2 2,WriteInstr 2 (DirAddr 0),WriteInstr 0 (DirAddr 1),EndProg,TestAndSet (DirAddr 1),Receive 2,Branch 2 (Rel 2),Jump (Rel (-3)),ReadInstr (DirAddr 0),Receive 2,Compute Incr 2 2 2,WriteInstr 2 (DirAddr 0),WriteInstr 0 (DirAddr 1),Load (ImmValue 103) 4,WriteInstr 4 (DirAddr 7),Jump (Rel 32),TestAndSet (DirAddr 2),Receive 2,Branch 2 (Rel 2),Jump (Rel (-3)),ReadInstr (DirAddr 3),Receive 2,Push 2,Load (ImmValue 1) 2,Push 2,Pop 3,Pop 2,Compute Sprockell.Mul 2 3 4,Push 4,Pop 2,WriteInstr 2 (DirAddr 3),ReadInstr (DirAddr 3),Receive 2,Push 2,Pop 2,WriteInstr 2 (DirAddr 65536),WriteInstr 0 (DirAddr 2),TestAndSet (DirAddr 1),Receive 2,Branch 2 (Rel 2),Jump (Rel (-3)),ReadInstr (DirAddr 0),Receive 2,Compute Decr 2 2 2,WriteInstr 2 (DirAddr 0),WriteInstr 0 (DirAddr 1),EndProg,ReadInstr (DirAddr 0),Receive 2,Branch 2 (Rel 2),Jump (Rel 2),Jump (Rel (-4)),Load (DirAddr 2) 2,Push 2,Pop 2,Branch 2 (Rel 4),Load (ImmValue 1) 3,Push 3,Jump (Rel 2),Push 0,Nop,Pop 2,Branch 2 (Rel 2),Jump (Rel 25),Load (DirAddr 1) 2,Push 2,Pop 2,WriteInstr 2 (DirAddr 65536),Load (DirAddr 1) 2,Push 2,Load (DirAddr 2) 2,Push 2,Pop 3,Pop 2,Compute Sprockell.Or 2 3 4,Push 4,Pop 2,WriteInstr 2 (DirAddr 65536),Load (DirAddr 1) 2,Push 2,Load (DirAddr 2) 2,Push 2,Pop 3,Pop 2,Compute Sprockell.And 2 3 4,Push 4,Pop 2,WriteInstr 2 (DirAddr 65536),Nop,Load (DirAddr 1) 2,Push 2,Pop 2,Branch 2 (Rel 2),Jump (Rel 30),ReadInstr (DirAddr 3),Receive 2,Push 2,Load (ImmValue 0) 2,Push 2,Pop 3,Pop 2,Compute LtE 2 3 4,Push 4,Pop 2,Branch 2 (Rel 13),ReadInstr (DirAddr 3),Receive 2,Push 2,Load (ImmValue 1) 2,Push 2,Pop 3,Pop 2,Compute Sprockell.Sub 2 3 4,Push 4,Pop 2,WriteInstr 2 (DirAddr 3),Jump (Rel 5),Load (ImmValue 0) 2,Push 2,Pop 2,Store 2 (DirAddr 1),Nop,Jump (Rel (-33)),Nop,ReadInstr (DirAddr 3),Receive 2,Push 2,Pop 2,WriteInstr 2 (DirAddr 65536),EndProg]

    it "Runtime" $ do
        setLocaleEncoding utf8
        input <- readFile "test.hola"
        case parseMyLang input of
            Left err -> error (show err)
            Right ast -> do
                let prog = codeGen ast
                run [prog, prog, prog, prog]
                pendingWith "Manual verification needed: should output something like the following:\n \tSprockell 1 says 1\n \tSprockell 3 says 1\n \tSprockell 2 says 2\n \tSprockell 0 says 1\n \tSprockell 0 says 1\n \tSprockell 0 says 0\n \tSprockell 0 says 0\n"


main :: IO ()
main = hspec $ do
        parserSpec
        typeCheckerSpec
        codeGenSpec
        runtimeSpec
        extendedTest