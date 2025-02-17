{-# LANGUAGE FlexibleInstances #-}

module Currycarbon.SumCalibration where

import           Currycarbon.Calibration.Calibration
import           Currycarbon.Calibration.Utils
import           Currycarbon.Types
import           Currycarbon.Utils

import           Data.Foldable                       (foldl')
import           Data.List                           (groupBy, sortBy)
import           Data.Ord                            (comparing)
import qualified Data.Vector.Unboxed                 as VU

evalNamedCalExpr :: CalibrationMethod -> CalibrateDatesConf -> CalCurveBP -> NamedCalExpr -> Either CurrycarbonException CalPDF
evalNamedCalExpr method conf curve (NamedCalExpr exprID expr) =
    case evalCalExpr method conf curve expr of
        Left err     -> Left err
        Right calPDF -> Right calPDF { _calPDFid = exprID }

-- | Evaluate a dating expression by calibrating the individual dates and forming the respective
--   sums and products of post-calibration density distributions.
--   Note that expressions are evaluated top-to-bottom. That renders it possible to perform trimming
--   and normalization selectively in the right order
evalCalExpr :: CalibrationMethod -> CalibrateDatesConf -> CalCurveBP -> CalExpr -> Either CurrycarbonException CalPDF
evalCalExpr method conf curve calExpr = norm $ evalE conf calExpr
    where
        evalE :: CalibrateDatesConf -> CalExpr -> Either CurrycarbonException CalPDF
        -- these are already normalized by their constructors
        evalE c (UnCalDate a)    = calibrateDate method c curve a
        evalE _ (WindowBP a)     = Right $ windowBP2CalPDF a
        evalE _ (WindowBCAD a)   = Right $ windowBCAD2CalPDF a
        -- this can theoretically be non-normalized input
        evalE _ (CalDate a)      = norm $ Right a
        -- sums must not be normalized
        evalE c (SumCal a b)     = eitherCombinePDFs (+) 0 (evalE c a) (evalE c b)
        -- products must be normalized (and their input, in case it's a sum)
        evalE c (ProductCal a b) = norm $ eitherCombinePDFs (*) 1 (productOne c a) (productOne c b)
        -- products between expressions can only be computed if the PDFs are not trimmed
        productOne c x = norm $ evalE (
             c {_calConfTrimCalCurveBeforeCalibration = False, _calConfTrimCalPDFAfterCalibration = False}
           ) x
        -- helper functions
        norm :: Either CurrycarbonException CalPDF -> Either CurrycarbonException CalPDF
        norm = mapEither id normalizeCalPDF

eitherCombinePDFs ::
    (Double -> Double -> Double) -> Double ->
    Either CurrycarbonException CalPDF ->
    Either CurrycarbonException CalPDF ->
    Either CurrycarbonException CalPDF
eitherCombinePDFs _ _ (Left e) _ = Left e
eitherCombinePDFs _ _ _ (Left e) = Left e
eitherCombinePDFs f initVal (Right a) (Right b) = Right $ combinePDFs f initVal a b

-- | Add two probability densities
addPDFs :: CalPDF -> CalPDF -> CalPDF
addPDFs = combinePDFs (+) 0

-- | Multiply two probability densities
multiplyPDFs :: CalPDF -> CalPDF -> CalPDF
multiplyPDFs = combinePDFs (*) 1

-- Combine probability densities
combinePDFs :: (Double -> Double -> Double) -> Double -> CalPDF -> CalPDF -> CalPDF
combinePDFs f initVal (CalPDF name1 cals1 dens1) (CalPDF name2 cals2 dens2) = do
    let minC1 = VU.minimum cals1
        minC2 = VU.minimum cals2
        maxC1 = VU.maximum cals1
        maxC2 = VU.maximum cals2
    if minC1 == minC2 && maxC1 == maxC2
    -- no aligning necessary
    then CalPDF (name1 ++ ";" ++ name2) cals1 (VU.zipWith f dens1 dens2)
    -- the PDFs have to be aligned to each other
    else
        let emptyC1 = getMiss minC1 maxC1 minC2 maxC2
            emptyC2 = getMiss minC2 maxC2 minC1 maxC1
            c1 = VU.toList (VU.zip cals1 dens1) ++ zip emptyC1 (repeat (0 :: Double))
            c2 = VU.toList (VU.zip cals2 dens2) ++ zip emptyC2 (repeat (0 :: Double))
            pdfSorted = sortBy (comparing fst) (c1 ++ c2)
            pdfGrouped = groupBy (\a b -> fst a == fst b) pdfSorted
            pdfRes = map foldYearGroup pdfGrouped
        in CalPDF (name1 ++ ";" ++ name2) (VU.fromList $ map fst pdfRes) (VU.fromList $ map snd pdfRes)
    where
        getMiss :: YearBCAD -> YearBCAD -> YearBCAD -> YearBCAD -> [YearBCAD]
        getMiss a1 a2 b1 b2
            | a1 <  b1 && a2 >  b2 = [a1..b1] ++ [b2..a2]
            | a1 <  b1 && a2 <= b2 = [a1..b1]
            | a1 >= b1 && a2 >  b2 = [b2..a2]
            | otherwise = []
        foldYearGroup :: [(YearBCAD, Double)] -> (YearBCAD, Double)
        foldYearGroup oneYear = (fst $ head oneYear, foldl' f initVal $ map snd oneYear)

-- | Create pseudo-CalPDF from RangeBCAD
windowBCAD2CalPDF :: TimeWindowBCAD -> CalPDF
windowBCAD2CalPDF (TimeWindowBCAD name start stop) =
    let years = VU.fromList [start..stop]
        dens = VU.replicate (VU.length years) 1
    in normalizeCalPDF $ CalPDF name years dens

windowBP2CalPDF :: TimeWindowBP -> CalPDF
windowBP2CalPDF (TimeWindowBP name start stop) =
    windowBCAD2CalPDF (TimeWindowBCAD name (bp2BCAD start) (bp2BCAD stop))
