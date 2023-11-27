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

evalNamedCalExpr :: CalibrateDatesConf -> CalCurveBP -> NamedCalExpr -> Either CurrycarbonException CalPDF
evalNamedCalExpr conf curve (NamedCalExpr exprID expr) =
    case evalCalExpr conf curve expr of
        Left err     -> Left err
        Right calPDF -> Right calPDF { _calPDFid = exprID }

-- | Evaluate a dating expression by calibrating the individual dates and forming the respective
--   sums and products of post-calibration density distributions
evalCalExpr :: CalibrateDatesConf -> CalCurveBP -> CalExpr -> Either CurrycarbonException CalPDF
evalCalExpr conf curve calExpr = mapEither id normalizeCalPDF $ evalE calExpr
    where
        evalE :: CalExpr -> Either CurrycarbonException CalPDF
        evalE (UnCalDate a)    = calibrateDate conf curve a
        evalE (WindowBP a)     = Right $ windowBP2CalPDF a
        evalE (WindowBCAD a)   = Right $ windowBCAD2CalPDF a
        evalE (CalDate a)      = Right a
        evalE (SumCal a b)     = eitherCombinePDFs (+) 0 (evalE a) (evalE b)
        evalE (ProductCal a b) = mapEither id normalizeCalPDF $ eitherCombinePDFs (*) 1
            (mapEither id normalizeCalPDF $ evalE a) (mapEither id normalizeCalPDF $ evalE b) -- product needs extra normalization
        -- https://hackage.haskell.org/package/either-5.0.2/docs/Data-Either-Combinators.html
        mapEither :: (a -> c) -> (b -> d) -> Either a b -> Either c d
        mapEither f _ (Left x)  = Left (f x)
        mapEither _ f (Right x) = Right (f x)

eitherCombinePDFs ::
    (Float -> Float -> Float) -> Float ->
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
combinePDFs :: (Float -> Float -> Float) -> Float -> CalPDF -> CalPDF -> CalPDF
combinePDFs f initVal (CalPDF name1 cals1 dens1) (CalPDF name2 cals2 dens2) =
        let minC1 = VU.minimum cals1
            minC2 = VU.minimum cals2
            maxC1 = VU.maximum cals1
            maxC2 = VU.maximum cals2
            emptyC1 = getMiss minC1 maxC1 minC2 maxC2
            emptyC2 = getMiss minC2 maxC2 minC1 maxC1
            c1 = VU.toList (VU.zip cals1 dens1) ++ zip emptyC1 (repeat (0 :: Float))
            c2 = VU.toList (VU.zip cals2 dens2) ++ zip emptyC2 (repeat (0 :: Float))
            pdfSorted = sortBy (comparing fst) (c1 ++ c2)
            pdfGrouped = groupBy (\a b -> fst a == fst b) pdfSorted
            pdfRes = map foldYearGroup pdfGrouped
        in CalPDF (name1 ++ ":" ++ name2) (VU.fromList $ map fst pdfRes) (VU.fromList $ map snd pdfRes)
        where
            getMiss :: YearBCAD -> YearBCAD -> YearBCAD -> YearBCAD -> [YearBCAD]
            getMiss a1 a2 b1 b2
                | a1 <  b1 && a2 >  b2 = [a1..b1] ++ [b2..a2]
                | a1 <  b1 && a2 <= b2 = [a1..b1]
                | a1 >= b1 && a2 >  b2 = [b2..a2]
                | otherwise = []
            foldYearGroup :: [(YearBCAD, Float)] -> (YearBCAD, Float)
            foldYearGroup oneYear = (fst $ head oneYear, foldl' f initVal $ map snd oneYear)

-- | Create pseudo-CalPDF from RangeBCAD
windowBCAD2CalPDF :: TimeWindowBCAD -> CalPDF
windowBCAD2CalPDF (TimeWindowBCAD name start stop) =
    let years = VU.fromList $ [start..stop]
        dens = VU.replicate (VU.length years) 1
    in CalPDF name years dens

windowBP2CalPDF :: TimeWindowBP -> CalPDF
windowBP2CalPDF (TimeWindowBP name start stop) =
    windowBCAD2CalPDF (TimeWindowBCAD name (bp2BCAD start) (bp2BCAD stop))
