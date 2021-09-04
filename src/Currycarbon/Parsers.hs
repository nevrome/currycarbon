module Currycarbon.Parsers where

import Currycarbon.Types

import           Data.List                      (intercalate, transpose)
import qualified Text.Parsec                    as P
import qualified Text.Parsec.String             as P
import Numeric.LinearAlgebra.Data as HM
import qualified Numeric.LinearAlgebra.HMatrix as HM
import qualified GHC.OldList as HM

-- * Parsing, rendering and writing functions
--
-- $importExport
--
-- This module contains a number of functions to manage data input and 
-- output plumbing for different datatypes

-- CalC14
writeCalC14s :: FilePath -> [CalC14] -> IO ()
writeCalC14s path calC14s = writeFile path $ 
    "sample,hdrSigma,hdrStart,hdrStop\n" 
    ++ intercalate "\n" (map renderCalC14ForFile calC14s)

renderCalC14ForFile :: CalC14 -> String
renderCalC14ForFile (CalC14 name hdrs68 hdrs95) =
    intercalate "\n" $ 
        map renderRow $
        zip3 (repeat name) (repeat "1") (renderHDRsForFile hdrs68) ++
        zip3 (repeat name) (repeat "2") (renderHDRsForFile hdrs95)
    where
        renderRow :: (String, String, (String, String)) -> String
        renderRow (a, b, (c, d)) = intercalate "," [a,b,c,d]

renderCalC14s :: [CalC14] -> String
renderCalC14s xs = 
    "Calibrated high density ranges (HDR):\n" 
    ++ intercalate "\n" (map renderCalC14 xs)

renderCalC14 :: CalC14 -> String
renderCalC14 (CalC14 name hdrs68 hdrs95) =
       "Sample: " ++ name ++ "\n" 
    ++ "1-sigma: " ++ renderHDRs (reverse hdrs68) ++ "\n"
    ++ "2-sigma: " ++ renderHDRs (reverse hdrs95)

-- HDR
renderHDRsForFile :: [HDR] -> [(String, String)]
renderHDRsForFile = map renderHDRForFile

renderHDRForFile :: HDR -> (String, String)
renderHDRForFile (HDR start stop) = (show stop, show start)

renderHDRs :: [HDR] -> String
renderHDRs xs = intercalate ", " (map renderHDR xs)

renderHDR :: HDR -> String
renderHDR (HDR stop start)
    | start < 0 && stop <= 0  = show (-start) ++ "-" ++ show (-stop) ++ "BC"
    | start < 0 && stop > 0   = show (-start) ++ "BC-" ++ show stop ++ "AD"
    | start >= 0 && stop >= 0 = show start ++ "-" ++ show stop ++ "AD"
    | otherwise = error $ "this should never happen: " ++ show start ++ "-" ++ show stop

-- CalCurveMatrix
writeCalCurveMatrixFile :: FilePath -> CalCurveMatrix -> IO ()
writeCalCurveMatrixFile path calCurveMatrix = 
    writeFile path $ renderCalCurveMatrixFile calCurveMatrix

renderCalCurveMatrixFile :: CalCurveMatrix -> String
renderCalCurveMatrixFile (CalCurveMatrix bps cals curveDensities) =
    let header = "," ++ intercalate "," (map show cals) ++ "\n"
        body = zipWith (\bp bpDens -> show bp ++ "," ++ intercalate "," (map show bpDens)) 
            bps (HM.toLists $ HM.tr $ curveDensities)
    in header ++ intercalate "\n" body

-- CalPDF
writeCalPDFs :: FilePath -> [CalPDF] -> IO ()
writeCalPDFs path calPDFs =
    writeFile path $ 
        "sample,calBCAD,density\n"
        ++ concatMap renderCalPDF calPDFs

renderCalPDF :: CalPDF -> String
renderCalPDF (CalPDF name obs) =
    concatMap (\(year,prob) -> show name ++ "," ++ show year ++ "," ++ show prob ++ "\n") obs

-- UncalC14
readUncalC14FromFile :: FilePath -> IO [UncalC14]
readUncalC14FromFile uncalFile = do
    s <- readFile uncalFile
    case P.runParser uncalC14SepByNewline () "" s of
        Left err -> error $ "Error in parsing dates from file: " ++ show err
        Right x -> return x
    where
        uncalC14SepByNewline :: P.Parser [UncalC14]
        uncalC14SepByNewline = P.endBy parseUncalC14 (P.newline <* P.spaces) <* P.eof

readUncalC14String :: String -> Either String [UncalC14]
readUncalC14String s = 
    case P.runParser uncalC14SepBySemicolon () "" s of
        Left err -> error $ "Error in parsing dates from string: " ++ show err
        Right x -> Right x
    where 
        uncalC14SepBySemicolon :: P.Parser [UncalC14]
        uncalC14SepBySemicolon = P.sepBy parseUncalC14 (P.char ';' <* P.spaces) <* P.eof

parseUncalC14 :: P.Parser UncalC14
parseUncalC14 = do
    P.try long P.<|> short
    where
        long = do
            name <- P.many (P.noneOf ",")
            _ <- P.oneOf ","
            mean <- read <$> P.many1 P.digit
            _ <- P.oneOf ","
            std <- read <$> P.many1 P.digit
            return (UncalC14 name mean std)
        short = do
            mean <- read <$> P.many1 P.digit
            _ <- P.oneOf ","
            std <- read <$> P.many1 P.digit
            return (UncalC14 "unknownSampleName" mean std)

-- CalCurve
writeCalCurveFile :: FilePath -> CalCurve -> IO ()
writeCalCurveFile path calCurve = 
    writeFile path $ renderCalCurve calCurve

renderCalCurve :: CalCurve -> String
renderCalCurve (CalCurve obs) =
    let header = "CAL BP,14C age,Sigma\n"
        body = map (\(x,y,z) -> show y ++ "," ++ show x ++ "," ++ show z) obs
    in header ++ intercalate "\n" body

readCalCurve :: FilePath -> IO CalCurve
readCalCurve calCurveFile = do
    calCurve <- readFile calCurveFile
    return $ loadCalCurve calCurve

loadCalCurve :: String -> CalCurve 
loadCalCurve calCurveString = do
    case P.runParser calCurveFileParser () "" calCurveString of
        Left p  -> error $ "This should never happen." ++ show p
        Right x -> CalCurve x

calCurveFileParser :: P.Parser [(Int, Int, Int)]
calCurveFileParser = do
    P.skipMany comments
    P.sepEndBy calCurveLineParser (P.manyTill P.anyToken (P.try P.newline))

calCurveLineParser :: P.Parser (Int, Int, Int) 
calCurveLineParser = do
  calBP <- read <$> P.many1 P.digit
  _ <- P.oneOf ","
  bp <- read <$> P.many1 P.digit
  _ <- P.oneOf ","
  sigma <- read <$> P.many1 P.digit
  return (bp, calBP, sigma)

comments :: P.Parser String
comments = do 
    _ <- P.string "#"
    _ <- P.manyTill P.anyChar P.newline
    return ""