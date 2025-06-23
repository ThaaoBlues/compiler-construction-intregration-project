import MyParser
import Test.Hspec
import Test.QuickCheck
import Data.Either
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

    it "parses a valid if statement" $ do
        let input = "si verdad { imprimir ¡5! :) }"
        parseMyLang input `shouldBe` Right [If (BoolLit True) [Print (IntLit 5)]]

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
                    If (BoolLit True) [Print (IntLit 5)]
                    ]
            checkTypeValid stmts

        it "accepts a valid array" $ do
            let stmts = [
                    Declaration Array "arr",
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
                    GlobalDecl "x"
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
                    If (IntLit 5) [Print (IntLit 5)]  -- Non-boolean condition
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
                    Declaration Array "arr",
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
                    GlobalDecl "x",
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

main :: IO ()
main = hspec $ do
    parserSpec
    typeCheckerSpec

-- main :: IO ()
-- main = hspec $ do
--   describe "Parsing" $ do
--     it "should parse numbers" $ do
--         property $ \n -> (parseMyLang $ show (getPositive n)) `shouldBe` (Right (getPositive n) :: Either String Integer)
-- main =
    -- --parseMyLang txt
--     case parseMyLang txt of
--          (Left err ) -> print err
--          (Right p) -> print p

--     where txt = "array x:) x = [1,2,3]:) durante x <= 5 { imprimir¡x!:) x = x*1*2 :)}"