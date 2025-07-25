{-# LANGUAGE TemplateHaskell #-}

module Currycarbon.CalCurves where

import           Currycarbon.ParserHelpers
import           Currycarbon.Types
import Currycarbon.Utils

import qualified Data.FileEmbed            as FE
import qualified Data.Vector.Unboxed       as VU
import qualified Text.Parsec               as P
import qualified Text.Parsec.String        as P
import Control.Exception (throwIO)

data CalCurveSelection =
      IntCal20 | SHCal20 | Marine20
    | CalCurveFromFile FilePath

instance Show CalCurveSelection where
    show IntCal20                = "IntCal20"
    show SHCal20                 = "SHCal20"
    show Marine20                = "Marine20"
    show (CalCurveFromFile path) = path

readCalCurveSelection :: String -> Either String CalCurveSelection
readCalCurveSelection s =
    case P.runParser parseCalCurveSelection () s s of
        Left err -> Left $ showParsecErr err
        Right x  -> Right x

parseCalCurveSelection :: P.Parser CalCurveSelection
parseCalCurveSelection =  do
    x <- P.many P.anyChar
    case x of
        "IntCal20" -> pure IntCal20
        "SHCal20"  -> pure SHCal20
        "Marine20" -> pure Marine20
        p          -> return $ CalCurveFromFile p

getCalCurve :: CalCurveSelection -> IO CalCurveBP
getCalCurve IntCal20                = pure intcal20
getCalCurve SHCal20                 = pure shcal20
getCalCurve Marine20                = pure marine20
getCalCurve (CalCurveFromFile path) = readCalCurveFromFile path

-- | Read a calibration curve file. The file must adhere to the .14c file format.
readCalCurveFromFile :: FilePath -> IO CalCurveBP
readCalCurveFromFile calCurveFile = do
    calCurveString <- readFile calCurveFile
    case readCalCurve calCurveString of
        Left err -> throwIO $ CurrycarbonCLIParsingException err
        Right x -> return x

readCalCurve :: String -> Either String CalCurveBP
readCalCurve calCurveString = do
    case P.runParser parseCalCurve () "calCurve" calCurveString of
        Left err -> Left $ showParsecErr err
        Right x  -> Right $ CalCurveBP
            (VU.fromList $ map (\(a,_,_) -> a) x)
            (VU.fromList $ map (\(_,b,_) -> b) x)
            (VU.fromList $ map (\(_,_,c) -> c) x)

readCalCurveUnsafe :: String -> CalCurveBP
readCalCurveUnsafe calCurveString = do
    case P.runParser parseCalCurve () "calCurve" calCurveString of
        Left err -> error $ "Error when reading calCurve " ++ show err
        Right x  -> CalCurveBP
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
intcal20 = readCalCurveUnsafe intcal20String
intcal20String :: String
intcal20String = $(FE.makeRelativeToProject "data/intcal20.14c" >>= FE.embedStringFile)

-- | The shcal20 calibration curve
-- (Hogg et al. 2020, doi: [10.1017/RDC.2020.59](https://doi.org/10.1017/RDC.2020.59))
shcal20 :: CalCurveBP
shcal20 = readCalCurveUnsafe shcal20String
shcal20String :: String
shcal20String = $(FE.makeRelativeToProject "data/shcal20.14c" >>= FE.embedStringFile)

-- | The shcal20 calibration curve
-- (Heaton et al. 2020, doi: [10.1017/RDC.2020.68](https://doi.org/10.1017/RDC.2020.68))
marine20 :: CalCurveBP
marine20 = readCalCurveUnsafe marine20String
marine20String :: String
marine20String = $(FE.makeRelativeToProject "data/marine20.14c" >>= FE.embedStringFile)
