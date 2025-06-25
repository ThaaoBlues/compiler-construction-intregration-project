import MyParser
import Test.Hspec
import Test.QuickCheck
import Data.Either
import MyCodeGen (codeGen)
import Sprockell (Instruction(..), RegAddr, MemAddr, AddrImmDI(..), Target(..), SprID,Operator(..), reg0, RegAddr, regA, regB, regC, regSprID, charIO)
-- Helper functions for type checker tests
checkTypeValid :: [Stmt] -> Expectation
checkTypeValid stmts = stackChecking stmts [fillSymbolTable stmts] `shouldBe` True

checkTypeInvalid :: [Stmt] -> Expectation
checkTypeInvalid stmts = stackChecking stmts [fillSymbolTable stmts] `shouldBe` False

-- Tests for the parser
parserSpec :: Spec
parserSpec = describe "Parser" $ do
    it "parses a valid declaration" $ do
        let input = "entero x :)"
        parseMyLang input `shouldBe` Right [Declaration Entero "x"]

    it "rejects an invalid declaration" $ do
        let input = "entero x"  -- Missing ":)"
        parseMyLang input `shouldSatisfy` isLeft

    it "parses a valid assignment" $ do
        let input = "x = 5 :)"
        parseMyLang input `shouldBe` Right [Assignment "x" (IntLit 5)]
    
    it "parses a valid array access" $ do
        let input = "array entero a:) a = [1,2,3]:) entero x:) x = a[1]:)"
        parseMyLang input `shouldBe` Right [Declaration (Array Entero) "a",Assignment "a" (ArrayLit [IntLit 1,IntLit 2,IntLit 3]),Declaration Entero "x",Assignment "x" (ArrayAccess "a" (IntLit 1))]

    it "parses a valid if statement" $ do
        let input = "si verdad { imprimir ¡5! :) } sino { imprimir ¡10! :) }"
        parseMyLang input `shouldBe` Right [If (BoolLit True) [Print (IntLit 5)] [Print (IntLit 10)]]

    it "rejects an invalid program" $ do
        let input = "x = 5"  -- Missing ":)"
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
                    Assignment "x" (BoolLit True)  -- Type error
                    ]
            checkTypeInvalid stmts

        it "rejects an invalid condition" $ do
            let stmts = [
                    If (IntLit 5) [Print (IntLit 5)] [Print (IntLit 7)]  -- Non-boolean condition
                    ]
            checkTypeInvalid stmts

        it "rejects an undefined variable" $ do
            let stmts = [
                    Assignment "x" (IntLit 5)  -- Undeclared variable
                    ]
            checkTypeInvalid stmts

        -- DO WE ACCEPT NON HOMOGENEOUS TYPES ?? I THINK SO BUT IT SEEMS WEIRD
        it "rejects an invalid array" $ do
            let stmts = [
                    Declaration (Array Entero) "arr",
                    Assignment "arr" (ArrayLit [IntLit 1, BoolLit True, IntLit 3])  -- Non-homogeneous types
                    ]
            checkTypeInvalid stmts




        it "rejects an invalid while loop condition" $ do
            let stmts = [
                    While (IntLit 5) [Print (IntLit 5)]  -- Non-boolean condition
                    ]
            checkTypeInvalid stmts

        it "rejects an invalid thread creation" $ do
            let stmts = [
                    ThreadCreate [Assignment "x" (IntLit 5)]  -- Undeclared variable
                    ]
            checkTypeInvalid stmts

        it "rejects an invalid global declaration" $ do
            let stmts = [
                    (Declaration (Global Entero) "x"),
                    Assignment "x" (BoolLit True)  -- Type error
                    ]
            checkTypeInvalid stmts

        it "rejects an invalid lock free" $ do
            let stmts = [
                    LockCreate "lock",
                    LockFree "lock2"  -- Undeclared lock
                    ]
            checkTypeInvalid stmts

        it "rejects an invalid lock free" $ do
            let stmts = [
                    LockFree "lock"  -- Undeclared lock
                    ]
            checkTypeInvalid stmts

        it "rejects an invalid lock get" $ do
            let stmts = [
                    LockGet "lock"  -- Undeclared lock
                    ]
            
            checkTypeInvalid stmts


codeGenSpec :: Spec
codeGenSpec = describe "Code generation" $ do
    describe "basic print program" $ do 
        it "accept basic print program" $ do 
            let prog = "imprimir¡5!:)"
            let ast = case parseMyLang prog of
                    (Left _) -> error "Parse error"
                    (Right tree) -> tree  

            let st = fillSymbolTable ast
            codeGen ast `shouldBe` [Load (ImmValue 5) 2,Push 2,Load (ImmValue 79) 2,WriteInstr 2 (DirAddr 65537),Load (ImmValue 85) 2,WriteInstr 2 (DirAddr 65537),Load (ImmValue 84) 2,WriteInstr 2 (DirAddr 65537),Load (ImmValue 32) 2,WriteInstr 2 (DirAddr 65537),Load (ImmValue 58) 2,WriteInstr 2 (DirAddr 65537),Load (ImmValue 32) 2,WriteInstr 2 (DirAddr 65537),Pop 2,WriteInstr 2 (DirAddr 65537)]



main :: IO ()
main = hspec $ do
    -- parserSpec
    -- typeCheckerSpec
    codeGenSpec
