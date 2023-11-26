module ParserSpec (spec) where

import Currycarbon.Parsers
import Currycarbon.Types

import           Test.Hspec    (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  testReadNamedExpression

testUncalC14 :: String -> CalExpr
testUncalC14 s = UnCalDate (UncalC14 s 3000 30)
testWindowBP :: String -> CalExpr
testWindowBP s = WindowBP (TimeWindowBP s 3000 2000)
testWindowBCAD :: String -> CalExpr
testWindowBCAD s = WindowBCAD (TimeWindowBCAD s (-1050) (-50))

testReadNamedExpression :: Spec
testReadNamedExpression =
    describe "Currycarbon.Parsers.readOneNamedCalExpr" $ do
        it "should read uncalibrated C14 dates correctly" $ do
            readOneNamedCalExpr "3000,30"
                `shouldBe`
                Right (NamedCalExpr "" (testUncalC14 ""))
            readOneNamedCalExpr "uncalC14(3000,30)"
                `shouldBe`
                Right (NamedCalExpr "" (testUncalC14 ""))
            readOneNamedCalExpr "uncalC14(test,3000,30)"
                `shouldBe`
                Right (NamedCalExpr "" (testUncalC14 "test"))
        it "should read time windows correctly" $ do
            readOneNamedCalExpr "rangeBP(3000,2000)"
                `shouldBe`
                Right (NamedCalExpr "" (testWindowBP ""))
            readOneNamedCalExpr "rangeBCAD(-1050,-50)"
                `shouldBe`
                Right (NamedCalExpr "" (testWindowBCAD ""))
            readOneNamedCalExpr "rangeBP(test,3000,2000)"
                `shouldBe`
                Right (NamedCalExpr "" (testWindowBP "test"))
            readOneNamedCalExpr "rangeBCAD(test,-1050,-50)"
                `shouldBe`
                Right (NamedCalExpr "" (testWindowBCAD "test"))
        it "should read simple sums correctly " $ do
            readOneNamedCalExpr "uncalC14(3000,30) + rangeBP(3000,2000)"
                `shouldBe`
                Right (NamedCalExpr "" $ SumCal (testUncalC14 "") (testWindowBP ""))
            readOneNamedCalExpr "uncalC14(3000,30) + rangeBP(3000,2000) + range"
                `shouldBe`
                Right (NamedCalExpr "" $ SumCal (testUncalC14 "") (testWindowBP ""))