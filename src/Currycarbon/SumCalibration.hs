{-# LANGUAGE FlexibleInstances #-}

module Currycarbon.SumCalibration where

import Currycarbon.Types
import Currycarbon.Utils
import Currycarbon.Calibration.Calibration
import Currycarbon.Parsers

import           Data.Foldable                  (foldl')
import qualified Data.Vector.Unboxed            as VU
import qualified Text.Parsec                    as P
import qualified Text.Parsec.String             as P
import Control.Exception (throwIO)

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

-- http://www.cse.chalmers.se/edu/year/2018/course/TDA452/lectures/RecursiveDataTypes.html
--data UnCalExpr =
--    | UncalDate UncalC14
--    | SumUnCal UnCalExpr UnCalExpr
--    | ProductUnCal UnCalExpr UnCalExpr

data CalExpr =
      UnCalDate UncalC14
    | CalDate CalPDF
    | SumCal CalExpr CalExpr
    | ProductCal CalExpr CalExpr
    deriving Show

evalCalExpr :: CalibrateDatesConf -> CalCurveBP -> CalExpr -> Maybe CalPDF
evalCalExpr conf curve (UnCalDate a)    = eitherToMaybe $ head $ calibrateDates conf curve [a]
evalCalExpr _    _     (CalDate a)      = Just a
evalCalExpr conf curve (SumCal a b)     = maybeCombinePDFs (+) (evalCalExpr conf curve a) (evalCalExpr conf curve b)
evalCalExpr conf curve (ProductCal a b) = maybeCombinePDFs (*) (evalCalExpr conf curve a) (evalCalExpr conf curve b)

--parseCalExpr :: P.Parser CalExpr
--parseCalExpr = do
--    parseUncalC14 

-- https://gist.github.com/abhin4v/017a36477204a1d57745
spaceChar :: Char -> P.Parser Char
spaceChar c = P.between P.spaces P.spaces (P.char c)
--spaceChar = P.char

add :: P.Parser CalExpr
add = SumCal <$> term <*> (spaceChar '+' *> expr)

mul :: P.Parser CalExpr
mul = ProductCal <$> factor <*> (spaceChar '*' *> term)

parens :: P.Parser CalExpr
parens = P.between (spaceChar '(') (spaceChar ')') expr

factor :: P.Parser CalExpr
factor = parens P.<|> (UnCalDate <$> parseUncalC14)

term :: P.Parser CalExpr
term = P.try mul P.<|> factor

expr :: P.Parser CalExpr
expr = P.try add P.<|> term -- <* P.eof

readCalExpr :: String -> Either String [CalExpr]
readCalExpr s =
    case P.runParser parseCalExprSepBySemicolon () "" s of
        Left err -> Left $ renderCurrycarbonException $ CurrycarbonCLIParsingException $ show err
        Right x -> Right x
        where
        parseCalExprSepBySemicolon :: P.Parser [CalExpr]
        parseCalExprSepBySemicolon = P.sepBy expr (P.char ';' <* P.spaces) <* P.eof

readCalExprFromFile :: FilePath -> IO [CalExpr]
readCalExprFromFile uncalFile = do
    s <- readFile uncalFile
    case P.runParser parseCalExprSepByNewline () "" s of
        Left err -> throwIO $ CurrycarbonCLIParsingException $ show err
        Right x -> return x
    where
        parseCalExprSepByNewline :: P.Parser [CalExpr]
        parseCalExprSepByNewline = P.endBy expr (P.newline <* P.spaces) <* P.eof

-- mconcat [Sum (head $ rights $ calibrateDates defaultCalConf intcal20 [(UncalC14 "a" 1000 30)]), Sum (head $ rights $ calibrateDates defaultCalConf intcal20 [(UncalC14 "a" 1000 30)]), Sum (head $ rights $ calibrateDates defaultCalConf intcal20 [(UncalC14 "a" 1000 30)])]

newtype Sum n = Sum n deriving Show

instance Semigroup (Sum CalPDF) where
    Sum pdf1 <> Sum pdf2 = Sum $ sumPDFs pdf1 pdf2

instance Monoid (Sum CalPDF) where
    mempty = Sum $ CalPDF mempty mempty mempty

newtype Product n = Product n deriving Show

instance Semigroup (Product CalPDF) where
    Product pdf1 <> Product pdf2 = Product $ multiplyPDFs pdf1 pdf2

instance Monoid (Product CalPDF) where
    mempty = Product $ CalPDF mempty mempty mempty

maybeCombinePDFs :: (Float -> Float -> Float) -> Maybe CalPDF -> Maybe CalPDF -> Maybe CalPDF
maybeCombinePDFs _ Nothing _ = Nothing
maybeCombinePDFs _ _ Nothing = Nothing
maybeCombinePDFs f (Just a) (Just b) = Just $ combinePDFs f a b

-- | Sum probabilty densities
sumPDFs :: CalPDF -> CalPDF -> CalPDF
sumPDFs = combinePDFs (+)

-- | Multiply probabilty densities
multiplyPDFs :: CalPDF -> CalPDF -> CalPDF
multiplyPDFs = combinePDFs (*)

-- Combine probability densities
combinePDFs :: (Float -> Float -> Float) -> CalPDF -> CalPDF -> CalPDF
combinePDFs f pdf1@(CalPDF name1 cals1 dens1) pdf2@(CalPDF name2 cals2 dens2) 
    | cals1 == mempty = pdf2
    | cals2 == mempty = pdf1
    | otherwise =
        let emptyRange = [(VU.last cals1+1)..(VU.head cals2-1)] ++ [(VU.last cals2+1)..(VU.head cals1-1)]
            pdfEmpty = zip emptyRange (repeat 0)
            pdfCombined = foldl' (fullOuter f) pdfEmpty [VU.toList $ VU.zip cals1 dens1, VU.toList $ VU.zip cals2 dens2]
            pdfNew = CalPDF (name1 ++ "+" ++ name2) (VU.fromList $ map fst pdfCombined) (VU.fromList $ map snd pdfCombined)
        in normalizeCalPDF pdfNew

-- https://stackoverflow.com/questions/24424403/join-or-merge-function-in-haskell
fullOuter :: (Float -> Float -> Float) -> [(YearBCAD, Float)] -> [(YearBCAD, Float)] -> [(YearBCAD, Float)]
fullOuter _ xs [] = xs
fullOuter _ [] ys = ys
fullOuter f xss@(x@(year1,dens1):xs) yss@(y@(year2,dens2):ys)
    | year1 == year2 = (year1, f dens1 dens2) : fullOuter f xs ys
    | year1 < year2  = x                      : fullOuter f xs yss
    | otherwise      = y                      : fullOuter f xss ys

