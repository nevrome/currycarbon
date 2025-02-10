{-# LANGUAGE TemplateHaskell #-}

module Currycarbon.CalCurves where

import           Currycarbon.ParserHelpers
import           Currycarbon.Types

import qualified Data.FileEmbed            as FE
import qualified Data.Vector.Unboxed       as VU
import qualified Text.Parsec               as P
import qualified Text.Parsec.String        as P

-- | Read a calibration curve file. The file must adhere to the current version of the
-- .c14 file format (e.g. [here](http://intcal.org/curves/intcal20.14c)). Look
-- [here](http://intcal.org/blurb.html) for other calibration curves
readCalCurveFromFile :: FilePath -> IO CalCurveBP
readCalCurveFromFile calCurveFile = do
    calCurve <- readFile calCurveFile
    return $ readCalCurve calCurve

readCalCurve :: String -> CalCurveBP
readCalCurve calCurveString = do
    case P.runParser parseCalCurve () "" calCurveString of
        Left p  -> error $ "This should never happen." ++ show p
        Right x -> CalCurveBP
            (VU.fromList $ map (\(a,_,_) -> a) x)
            (VU.fromList $ map (\(_,b,_) -> b) x)
            (VU.fromList $ map (\(_,_,c) -> c) x)

parseCalCurve :: P.Parser [(YearBP, YearBP, YearRange)]
parseCalCurve = do
    P.skipMany comments
    P.sepEndBy parseCalCurveLine (P.manyTill P.anyToken (P.try P.newline))

parseCalCurveLine :: P.Parser (YearBP, YearBP, YearRange)
parseCalCurveLine = do
  calBP <- parseWord
  _ <- P.oneOf ","
  bp <- parseWord
  _ <- P.oneOf ","
  sigma <- parseWord
  return (calBP, bp, sigma)

comments :: P.Parser String
comments = do
    _ <- P.string "#"
    _ <- P.manyTill P.anyChar P.newline
    return ""

-- | The intcal20 calibration curve
-- (Reimer et al. 2020, doi: [10.1017/RDC.2020.41](https://doi.org/10.1017/RDC.2020.41))
intcal20 :: CalCurveBP
intcal20 = readCalCurve intcal20String
intcal20String :: String
intcal20String = $(FE.embedStringFile "data/intcal20.14c")
