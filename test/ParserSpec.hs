module ParserSpec (spec) where

import           Currycarbon.Parsers
import           Currycarbon.Types

import           Test.Hspec          (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  testReadNamedExpression

uncalC14N :: String -> CalExpr
uncalC14N s = UnCalDate (UncalC14 s 3000 30)
windowBPN :: String -> CalExpr
windowBPN s = WindowBP (TimeWindowBP s 3000 2000)
windowBCADN :: String -> CalExpr
windowBCADN s = WindowBCAD (TimeWindowBCAD s (-1050) (-50))
uncalC14 :: CalExpr
uncalC14 = uncalC14N ""
windowBP :: CalExpr
windowBP = windowBPN ""
windowBCAD :: CalExpr
windowBCAD = windowBCADN ""

testReadNamedExpression :: Spec
testReadNamedExpression =
    describe "Currycarbon.Parsers.readOneNamedCalExpr" $ do
    it "should read uncalibrated C14 dates correctly" $ do
        readOneNamedCalExpr "3000,30"
            `shouldBe`
            Right (NamedCalExpr "" uncalC14)
        readOneNamedCalExpr "uncalC14(3000,30)"
            `shouldBe`
            Right (NamedCalExpr "" uncalC14)
        readOneNamedCalExpr "uncalC14(test,3000,30)"
            `shouldBe`
            Right (NamedCalExpr "" (uncalC14N "test"))
    it "should read named function arguments correctly" $ do
        readOneNamedCalExpr "uncalC14(id = test, yearBP = 3000, sigma = 30)"
            `shouldBe`
            Right (NamedCalExpr "" (uncalC14N "test"))
    it "should read partially named function arguments correctly" $ do
        readOneNamedCalExpr "uncalC14(3000,sigma=30)"
            `shouldBe`
            Right (NamedCalExpr "" uncalC14)
    it "should read time windows correctly" $ do
        readOneNamedCalExpr "rangeBP(3000,2000)"
            `shouldBe`
            Right (NamedCalExpr "" windowBP)
        readOneNamedCalExpr "rangeBCAD(-1050,-50)"
            `shouldBe`
            Right (NamedCalExpr "" windowBCAD)
        readOneNamedCalExpr "rangeBP(test,3000,2000)"
            `shouldBe`
            Right (NamedCalExpr "" (windowBPN "test"))
        readOneNamedCalExpr "rangeBCAD(test,-1050,-50)"
            `shouldBe`
            Right (NamedCalExpr "" (windowBCADN "test"))
    it "should read sums with + operator correctly " $ do
        readOneNamedCalExpr "uncalC14(3000,30) + rangeBP(3000,2000)"
            `shouldBe`
            Right (NamedCalExpr "" $ SumCal uncalC14 windowBP)
        readOneNamedCalExpr "uncalC14(3000,30) + rangeBP(3000,2000) + rangeBCAD(-1050,-50)"
            `shouldBe`
            Right (NamedCalExpr "" $ SumCal uncalC14 (SumCal windowBP windowBCAD))
        readOneNamedCalExpr "uncalC14(3000,30) + rangeBP(3000,2000) + rangeBCAD(-1050,-50) + uncalC14(3000,30)"
            `shouldBe`
            Right (NamedCalExpr "" $ SumCal uncalC14 (SumCal windowBP (SumCal windowBCAD uncalC14)))
    it "should read sums with sum() function and + operator correctly " $ do
        readOneNamedCalExpr "sum(uncalC14(3000,30), rangeBP(3000,2000))"
            `shouldBe`
            Right (NamedCalExpr "" $ SumCal uncalC14 windowBP)
        readOneNamedCalExpr "sum(uncalC14(3000,30), rangeBP(3000,2000)) + rangeBCAD(-1050,-50)"
            `shouldBe`
            Right (NamedCalExpr "" $ SumCal (SumCal uncalC14 windowBP) windowBCAD)
        readOneNamedCalExpr "uncalC14(3000,30) + sum(rangeBP(3000,2000), rangeBCAD(-1050,-50)) + uncalC14(3000,30)"
            `shouldBe`
            Right (NamedCalExpr "" $ SumCal uncalC14 (SumCal (SumCal windowBP windowBCAD) uncalC14))
    it "should read products with * operator correctly " $ do
        readOneNamedCalExpr "uncalC14(3000,30) * rangeBP(3000,2000)"
            `shouldBe`
            Right (NamedCalExpr "" $ ProductCal uncalC14 windowBP)
        readOneNamedCalExpr "uncalC14(3000,30) * rangeBP(3000,2000) * rangeBCAD(-1050,-50)"
            `shouldBe`
            Right (NamedCalExpr "" $ ProductCal uncalC14 (ProductCal windowBP windowBCAD))
        readOneNamedCalExpr "uncalC14(3000,30) * rangeBP(3000,2000) * rangeBCAD(-1050,-50) * uncalC14(3000,30)"
            `shouldBe`
            Right (NamedCalExpr "" $ ProductCal uncalC14 (ProductCal windowBP (ProductCal windowBCAD uncalC14)))
    it "should read products with product() function and * operator correctly " $ do
        readOneNamedCalExpr "product(uncalC14(3000,30), rangeBP(3000,2000))"
            `shouldBe`
            Right (NamedCalExpr "" $ ProductCal uncalC14 windowBP)
        readOneNamedCalExpr "product(uncalC14(3000,30), rangeBP(3000,2000)) * rangeBCAD(-1050,-50)"
            `shouldBe`
            Right (NamedCalExpr "" $ ProductCal (ProductCal uncalC14 windowBP) windowBCAD)
        readOneNamedCalExpr "uncalC14(3000,30) * product(rangeBP(3000,2000), rangeBCAD(-1050,-50)) * uncalC14(3000,30)"
            `shouldBe`
            Right (NamedCalExpr "" $ ProductCal uncalC14 (ProductCal (ProductCal windowBP windowBCAD) uncalC14))
    it "should understand parenthesis correctly" $ do
        readOneNamedCalExpr "(uncalC14(3000,30) + rangeBP(3000,2000)) * rangeBCAD(-1050,-50)"
            `shouldBe`
            Right (NamedCalExpr "" $ ProductCal (SumCal uncalC14 windowBP) windowBCAD)
    it "should read unnamed and named calibration expressions correctly" $ do
        readOneNamedCalExpr "test: 3000,30"
            `shouldBe`
            Right (NamedCalExpr "test" uncalC14)
        readOneNamedCalExpr "calExpr(test,3000,30)"
            `shouldBe`
            Right (NamedCalExpr "test" uncalC14)
        readOneNamedCalExpr "calExpr(3000,30)"
            `shouldBe`
            Right (NamedCalExpr "" uncalC14)
    it "should be able to handle complex, nested queries" $ do
        readOneNamedCalExpr "calExpr(test, sum(uncalC14(3000,30), product(rangeBP(3000,2000), rangeBCAD(-1050,-50))) * uncalC14(3000,30))"
            `shouldBe`
            Right (NamedCalExpr "test" $ ProductCal (SumCal uncalC14 (ProductCal windowBP windowBCAD)) uncalC14)

