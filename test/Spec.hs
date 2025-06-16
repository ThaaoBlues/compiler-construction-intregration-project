import MyParser
import Test.Hspec
import Test.QuickCheck

main :: IO ()
-- main = hspec $ do
--   describe "Parsing" $ do
--     it "should parse numbers" $ do
--         property $ \n -> (parseMyLang $ show (getPositive n)) `shouldBe` (Right (getPositive n) :: Either String Integer)
main =
    -- --parseMyLang txt
    case parseMyLang txt of
         (Left err ) -> print err
         (Right p) -> print p

    where txt = "array x:) x = [1,2,3]:) durante x <= 5 { imprimir¡x!:) x = x*1*2 :)}"