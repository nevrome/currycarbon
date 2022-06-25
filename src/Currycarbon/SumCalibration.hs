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
import Data.List (intercalate, sortBy, group, groupBy)
import Data.Ord (comparing)

evalCalExpr :: CalibrateDatesConf -> CalCurveBP -> CalExpr -> Either CurrycarbonException CalPDF
evalCalExpr conf curve calExpr = mapEither id normalizeCalPDF $ evalE calExpr
    where
        evalE :: CalExpr -> Either CurrycarbonException CalPDF
        evalE (UnCalDate a)    = calibrateDate conf curve a
        evalE (CalDate a)      = Right a
        evalE (SumCal a b)     = eitherCombinePDFs (+) 0 (evalE a) (evalE b)
        evalE (ProductCal a b) = mapEither id normalizeCalPDF $ eitherCombinePDFs (*) 1 
            (mapEither id normalizeCalPDF $ evalE a) (mapEither id normalizeCalPDF $ evalE b) -- product needs extra normalization
        -- https://hackage.haskell.org/package/either-5.0.2/docs/Data-Either-Combinators.html
        mapEither :: (a -> c) -> (b -> d) -> Either a b -> Either c d
        mapEither f _ (Left x)  = Left (f x)
        mapEither _ f (Right x) = Right (f x)

eitherCombinePDFs :: (Float -> Float -> Float) -> Float -> Either CurrycarbonException CalPDF -> Either CurrycarbonException CalPDF -> Either CurrycarbonException CalPDF
eitherCombinePDFs _ _ (Left e) _ = Left e
eitherCombinePDFs _ _ _ (Left e) = Left e
eitherCombinePDFs f initVal (Right a) (Right b) = Right $ combinePDFs f initVal a b

-- | Sum probabilty densities
sumPDFs :: CalPDF -> CalPDF -> CalPDF
sumPDFs = combinePDFs (+) 0

-- | Multiply probabilty densities
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


-- -- Combine probability densities
-- combinePDFs :: (Float -> Float -> Float) -> CalPDF -> CalPDF -> CalPDF
-- combinePDFs f (CalPDF name1 cals1 dens1) (CalPDF name2 cals2 dens2) =
--         let emptyRange = [(VU.last cals1+1)..(VU.head cals2-1)] ++ [(VU.last cals2+1)..(VU.head cals1-1)]
--             pdfEmpty = zip emptyRange (repeat 0.0)
--             pdfCombined = foldl' (fullOuter f) pdfEmpty [VU.toList $ VU.zip cals1 dens1, VU.toList $ VU.zip cals2 dens2]
--             pdfNew = CalPDF (name1 ++ ":" ++ name2) (VU.fromList $ map fst pdfCombined) (VU.fromList $ map snd pdfCombined)
--         in error $ show pdfEmpty--pdfNew

-- -- https://stackoverflow.com/questions/24424403/join-or-merge-function-in-haskell
-- fullOuter :: (Float -> Float -> Float) -> [(YearBCAD, Float)] -> [(YearBCAD, Float)] -> [(YearBCAD, Float)]
-- fullOuter _ xs [] = xs
-- fullOuter _ [] ys = ys
-- fullOuter f xss@(x@(year1,dens1):xs) yss@(y@(year2,dens2):ys)
--     | year1 == year2 = (year1, f dens1 dens2) : fullOuter f xs ys
--     | year1 < year2  = x                      : fullOuter f xs yss
--     | otherwise      = y                      : fullOuter f xss ys

