module Currycarbon.Parsers where

import Currycarbon.Types

import           Data.List                      (intercalate, transpose)
import qualified Text.Parsec                    as P
import qualified Text.Parsec.String             as P

-- CalC14
writeCalC14 :: FilePath -> [CalC14] -> IO ()
writeCalC14 path calC14 = writeFile path $ renderCalC14s calC14

renderCalC14s :: [CalC14] -> String
renderCalC14s xs = 
    "Calibrated high density ranges (HDR):\n" ++
    intercalate "\n" (map renderCalC14 xs)

renderCalC14 :: CalC14 -> String
renderCalC14 (CalC14 name hdrs68 hdrs95) =
       "Sample: " ++ name ++ "\n" 
    ++ "1-sigma: " ++ renderHDRs (reverse hdrs68) ++ "\n"
    ++ "2-sigma: " ++ renderHDRs (reverse hdrs95)

-- HDR
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
            bps (transpose curveDensities)
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
    let multiEntityParser = uncalC14Parser `P.sepBy1` (P.newline *> P.spaces)
    eitherParseResult <- P.parseFromFile (P.spaces *> multiEntityParser <* P.spaces) uncalFile
    case eitherParseResult of
        Left err -> error $ "Error in parsing dates from File: " ++ show err
        Right r -> return (concat r)

readUncalC14String :: String -> Either String [UncalC14]
readUncalC14String s = case P.runParser uncalC14Parser () "" s of
    Left p  -> Left (show p)
    Right x -> Right x

uncalC14Parser :: P.Parser [UncalC14]
uncalC14Parser = P.try (P.sepBy parseOneUncalC14 (P.char ';' <* P.spaces))

parseOneUncalC14 :: P.Parser UncalC14
parseOneUncalC14 = do
    name <- P.option "unknownSampleName" (P.between (P.char '[') (P.char ']') (P.manyTill P.anyChar (P.lookAhead (P.char ']'))))
    mean <- read <$> P.many1 P.digit
    _ <- P.oneOf "+"
    std <- read <$> P.many1 P.digit
    return (UncalC14 name mean std)

-- CalCurve
writeCalCurveFile :: FilePath -> CalCurve -> IO ()
writeCalCurveFile path calCurve = 
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
    _ <- P.string "#"
    _ <- P.manyTill P.anyChar P.newline
    return ""