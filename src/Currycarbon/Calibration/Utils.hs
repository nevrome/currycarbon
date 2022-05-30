{-# LANGUAGE Strict #-}

module Currycarbon.Calibration.Utils where

import Currycarbon.Types

import qualified Data.Vector.Unboxed as VU
import Data.Maybe (fromMaybe)
import Numeric.SpecFunctions (logBeta)

instance Semigroup CalPDF where
  pdf1 <> pdf2 = sumPDFs pdf1 pdf2

instance Monoid CalPDF where
    mempty = CalPDF "" VU.empty VU.empty

-- | Sum probabilty densities
sumPDFs :: CalPDF -> CalPDF -> CalPDF
sumPDFs = combinePDFs (+)

-- | Multiply probabilty densities
multiplyPDFs :: CalPDF -> CalPDF -> CalPDF
multiplyPDFs = combinePDFs (*)

-- | Combine probability densities
-- CalPDF could be an instance of Semigroup, or even Monoid, but unfortunately mempty depends on the input PDFs
combinePDFs :: (Float -> Float -> Float) -> CalPDF -> CalPDF -> CalPDF
combinePDFs f (CalPDF name1 cals1 dens1) (CalPDF name2 cals2 dens2) = 
    let startRange = minimum [VU.head cals1, VU.head cals2]
        stopRange = maximum [VU.last cals1, VU.last cals2]
        emptyBackdrop = zip [startRange..stopRange] (repeat (0.0 :: Float))
        pdf1 = VU.toList $ VU.zip cals1 dens1
        pdf2 = VU.toList $ VU.zip cals2 dens2
        pdfCombined = fullOuter f pdf2 (fullOuter f pdf1 emptyBackdrop)
        pdfNew = CalPDF (name1 ++ "+" ++ name2) (VU.fromList $ map fst pdfCombined) (VU.fromList $ map snd pdfCombined)
    in normalizeCalPDF pdfNew
    where
        -- https://stackoverflow.com/questions/24424403/join-or-merge-function-in-haskell
        fullOuter :: (Float -> Float -> Float) -> [(YearBCAD, Float)] -> [(YearBCAD, Float)] -> [(YearBCAD, Float)]
        fullOuter _ xs [] = xs
        fullOuter _ [] ys = ys
        fullOuter f xss@(x@(year1,dens1):xs) yss@(y@(year2,dens2):ys)
            | year1 == year2 = (year1, f dens1 dens2) : fullOuter f xs ys
            | year1 < year2  = x                      : fullOuter f xs yss
            | otherwise      = y                      : fullOuter f xss ys

-- | get the density of a normal distribution at a point x
-- 
-- >>> dnorm 1.0 1.0 1.0
-- 0.3989423
dnorm :: Float -> Float -> Float -> Float 
dnorm mu sigma x = 
    let a = recip (sqrt (2 * pi * sigma2))
        b = exp (-c2 / (2 * sigma2))
        c = x - mu
        c2 = c * c
        sigma2 = sigma * sigma
    in a*b
    -- alternative implemenation with the statistics package:
    -- import Statistics.Distribution (density)
    -- realToFrac $ density (normalDistr (realToFrac mu) (realToFrac sigma)) (realToFrac x)

-- | get the density of student's-t distribution at a point x
-- 
-- >>> dt 1.0 1.0
-- 0.15915494
dt :: Double -> Float -> Float
dt dof x =
    let xDouble = realToFrac x
        logDensityUnscaled = log (dof / (dof + xDouble*xDouble)) * (0.5 * (1 + dof)) - logBeta 0.5 (0.5 * dof)
    in realToFrac $ exp logDensityUnscaled / sqrt dof
    -- alternative implemenation with the statistics package:
    -- import Statistics.Distribution.StudentT (studentT)
    -- realToFrac $ density (studentT (realToFrac dof)) (realToFrac x) -- dof: number of degrees of freedom

isOutsideRangeOfCalCurve :: CalCurveBP -> UncalC14 -> Bool
isOutsideRangeOfCalCurve (CalCurveBP _ uncals _) (UncalC14 _ age _) = 
    age < VU.minimum uncals || age > VU.maximum uncals

-- | Take an uncalibrated date and a raw calibration curve and return
-- the relevant segment of the calibration curve
getRelevantCalCurveSegment :: UncalC14 -> CalCurveBP -> CalCurveBP
getRelevantCalCurveSegment (UncalC14 _ mean std) (CalCurveBP cals uncals sigmas) =
    let start = mean+6*std
        stop = mean-6*std
        startIndex = fromMaybe 0 $ VU.findIndex (<= start) uncals
        stopIndex = (VU.length uncals - 1) - fromMaybe 0 (VU.findIndex (>= stop) $ VU.reverse uncals)
        toIndex = stopIndex - startIndex
    in CalCurveBP (VU.slice startIndex toIndex cals) (VU.slice startIndex toIndex uncals) (VU.slice startIndex toIndex sigmas)

-- | Modify a calibration curve (segment) with multiple optional steps, 
-- including interpolation and transforming dates to BC/AD format
prepareCalCurveSegment :: Bool -> CalCurveBP -> CalCurveBCAD
prepareCalCurveSegment interpolate calCurve =
    makeBCADCalCurve $ if interpolate then interpolateCalCurve calCurve else calCurve

makeBCADCalCurve :: CalCurveBP -> CalCurveBCAD
makeBCADCalCurve (CalCurveBP cals uncals sigmas) = CalCurveBCAD (vectorBPToBCAD cals) (vectorBPToBCAD uncals) sigmas

vectorBPToBCAD :: VU.Vector YearBP -> VU.Vector YearBCAD
vectorBPToBCAD = VU.map (\x -> -(fromIntegral x) + 1950)

interpolateCalCurve :: CalCurveBP -> CalCurveBP
interpolateCalCurve (CalCurveBP cals uncals sigmas) =
    let obs = VU.zip3 cals uncals sigmas
        timeWindows = getTimeWindows obs
        obsFilled = VU.concatMap fillTimeWindows timeWindows
    in uncurry3 CalCurveBP $ VU.unzip3 obsFilled
    where
        getTimeWindows :: VU.Vector (YearBP,YearBP,YearRange) -> VU.Vector ((YearBP,YearBP,YearRange),(YearBP,YearBP,YearRange))
        getTimeWindows xs = VU.zipWith (,) (VU.init xs) (VU.tail xs)
        fillTimeWindows :: ((YearBP,YearBP,YearRange),(YearBP,YearBP,YearRange)) -> VU.Vector (YearBP,YearBP,YearRange)
        fillTimeWindows ((calbp1,bp1,sigma1),(calbp2,bp2,sigma2)) =
            if calbp1 == calbp2 || calbp1+1 == calbp2 || calbp1-1 == calbp2 
            then VU.singleton (calbp1,bp1,sigma1)
            else 
                let newCals = VU.fromList [calbp1,calbp1-1..calbp2+1] -- range definition like this to trigger counting down
                    newBPs = VU.map (snd . getInBetweenPointsInt (calbp1,bp1) (calbp2,bp2)) newCals
                    newSigmas = VU.map (snd . getInBetweenPointsInt (calbp1,sigma1) (calbp2,sigma2)) newCals
                in VU.zip3 newCals newBPs newSigmas
        getInBetweenPointsInt :: (Word, Word) -> (Word, Word) -> Word -> (Word, Word)
        getInBetweenPointsInt (x1,y1) (x2,y2) xPred =
            let (_,yPred) = getInBetweenPoints (fromIntegral x1,fromIntegral y1) (fromIntegral x2,fromIntegral y2) $ fromIntegral xPred
            in (xPred, round yPred)
        getInBetweenPoints :: (Float, Float) -> (Float, Float) -> Float -> (Float, Float)
        getInBetweenPoints (x1,y1) (x2,y2) xPred =
            let yDiff = y2 - y1
                xDiff = abs $ x1 - x2
                yDiffPerxDiff = yDiff/xDiff
                xPredRel = x1 - xPred
            in (xPred, y1 + xPredRel * yDiffPerxDiff)
        uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
        uncurry3 f ~(a,b,c) = f a b c

normalizeCalPDF :: CalPDF -> CalPDF
normalizeCalPDF (CalPDF name cals dens) = 
    let sumDens = VU.sum dens
        normalizedDens = VU.map (/ sumDens) dens
    in CalPDF name cals normalizedDens

trimLowDensityEdgesCalPDF :: CalPDF -> CalPDF
trimLowDensityEdgesCalPDF (CalPDF name cals dens) =
    let firstAboveThreshold = fromMaybe 0 (VU.findIndex (> 0.00001) dens)
        lastAboveThreshold = fromMaybe 0 (VU.findIndex (> 0.00001) $ VU.reverse dens)
        untilLastAboveThreshold = VU.length dens - firstAboveThreshold - lastAboveThreshold
        calsSlice = VU.slice firstAboveThreshold untilLastAboveThreshold cals
        densSlice = VU.slice firstAboveThreshold untilLastAboveThreshold dens
    in CalPDF name calsSlice densSlice
