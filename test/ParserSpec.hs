module ParserSpec (spec) where

import Currycarbon.Parsers
import Currycarbon.Types

import           Test.Hspec    (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  testReadNamedExpression

testReadNamedExpression :: Spec
testReadNamedExpression =
    describe "Currycarbon.Parsers.readOneNamedCalExpr" $ do
        it "should read uncalibrated C14 dates correctly" $ do
            readOneNamedCalExpr "3000,30"
                `shouldBe`
                Right (NamedCalExpr "" (UnCalDate (UncalC14 "" 3000 30)))
            readOneNamedCalExpr "uncalC14(3000,30)"
                `shouldBe`
                Right (NamedCalExpr "" (UnCalDate (UncalC14 "" 3000 30)))
            readOneNamedCalExpr "uncalC14(test,3000,30)"
                `shouldBe`
                Right (NamedCalExpr "" (UnCalDate (UncalC14 "test" 3000 30)))
        it "should read time windows correctly" $ do
            readOneNamedCalExpr "rangeBP(3000,2000)"
                `shouldBe`
                Right (NamedCalExpr "" (WindowBP (TimeWindowBP "" 3000 2000)))
            readOneNamedCalExpr "rangeBCAD(-3000,-2000)"
                `shouldBe`
                Right (NamedCalExpr "" (WindowBCAD (TimeWindowBCAD "" (-3000) (-2000))))
            readOneNamedCalExpr "rangeBP(test,3000,2000)"
                `shouldBe`
                Right (NamedCalExpr "" (WindowBP (TimeWindowBP "test" 3000 2000)))
            readOneNamedCalExpr "rangeBCAD(test,-3000,-2000)"
                `shouldBe`
                Right (NamedCalExpr "" (WindowBCAD (TimeWindowBCAD "test" (-3000) (-2000))))