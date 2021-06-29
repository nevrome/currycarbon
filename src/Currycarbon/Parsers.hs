module Currycarbon.Parsers where

import Currycarbon.Types
import Currycarbon.Utils

import           Control.Exception              (throwIO)
import           Control.Monad                  (guard)
import           Data.List                      (intercalate, transpose)
import qualified Text.Parsec                    as P
import qualified Text.Parsec.String             as P
import qualified Text.Parsec.Number             as P

-- CalCurveMatrix
writeCalCurveMatrixFile :: FilePath -> CalCurveMatrix -> IO ()
writeCalCurveMatrixFile path calCurveMatrix = 
    writeFile path $ renderCalCurveMatrixFile calCurveMatrix

renderCalCurveMatrixFile :: CalCurveMatrix -> String
renderCalCurveMatrixFile (CalCurveMatrix bps cals curveDensities) =
    let header = "," ++ intercalate "," (map show cals) ++ "\n"
        body = zipWith (\bp bpDens -> show bp ++ "," ++ intercalate "," (map show bpDens)) 
            bps (transpose curveDensities)
    in header ++ intercalate "\n" body

-- CalPDF
writeCalPDFs :: FilePath -> [CalPDF] -> IO ()
writeCalPDFs path calPDFs =
    writeFile path $ 
        "sample,calBC,density\n"
        ++ concatMap renderCalPDF calPDFs

renderCalPDF :: CalPDF -> String
renderCalPDF (CalPDF name obs) =
    concatMap (\(year,prob) -> show name ++ "," ++ show year ++ "," ++ show prob ++ "\n") obs

-- UncalC14
readUncalC14String :: String -> Either String [UncalC14]
readUncalC14String s = case P.runParser uncalC14Parser () "" s of
    Left p  -> Left (show p)
    Right x -> Right x

uncalC14Parser :: P.Parser [UncalC14]
uncalC14Parser = P.try (P.sepBy parseOneUncalC14 (P.char ';' <* P.spaces))

parseOneUncalC14 :: P.Parser UncalC14
parseOneUncalC14 = do
    name <- P.manyTill P.anyChar $ P.string ":"
    mean <- read <$> P.many1 P.digit
    _ <- P.oneOf "+"
    std <- read <$> P.many1 P.digit
    return (UncalC14 name mean std)

-- CalCurve
writeCalCurve :: FilePath -> CalCurve -> IO ()
writeCalCurve path calCurve = 
    writeFile path $ renderCalCurve calCurve

renderCalCurve :: CalCurve -> String
renderCalCurve (CalCurve obs) =
    let header = "CAL BP,14C age,Sigma\n"
        body = map (\(x,y,z) -> show y ++ "," ++ show x ++ "," ++ show z) obs
    in header ++ intercalate "\n" body

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
    P.string "#"
    comment <- P.manyTill P.anyChar P.newline
    return ""