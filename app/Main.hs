module Main where

import MyParser (parseMyLang)
import MyCodeGen (codeGen)
import Sprockell (run, Instruction)

-- Compiles a number into a spril program producing all fibonacci numbers below the number
-- Compilation might fail
--compile :: String -> Either String [Instruction]
compile = 1
-- compile txt = do
    -- ast <- parseMyLang txt

    -- -- --parseMyLang txt
    -- case ast of
    --      (Left err) -> error "Malformed program"
    --      (Right p) -> pure $ codeGen p


-- Gets a number and runs the resulting spril program of compilation succeeds
main :: IO ()
main = pure ()
-- main = do
--     txt <- getLine
--     case compile txt of
--          (Left err) -> fail err
--          (Right spril) -> run [spril]
