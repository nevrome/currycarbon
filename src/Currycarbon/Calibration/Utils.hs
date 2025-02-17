{-# LANGUAGE Strict #-}

module Currycarbon.Calibration.Utils where

import           Currycarbon.Types

import           Data.Maybe            (fromMaybe)
import qualified Data.Vector.Unboxed   as VU
import           Numeric.SpecFunctions (logBeta)

-- https://hackage.haskell.org/package/either-5.0.2/docs/Data-Either-Combinators.html
mapEither :: (a -> c) -> (b -> d) -> Either a b -> Either c d
mapEither f _ (Left x)  = Left (f x)
mapEither _ f (Right x) = Right (f x)

-- | Rescale a CalPDF so that the sum of the densities is approx. 1.0
normalizeCalPDF :: CalPDF -> CalPDF
normalizeCalPDF (CalPDF name cals dens) =
    case VU.sum dens of
      0.0 -> CalPDF name cals dens -- product calibration can yield empty calPDFs
      s   -> CalPDF name cals $ VU.map (/s) dens

-- | get the density of a normal distribution at a point x
dnorm :: Double -> Double -> Double -> Double
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
dt :: Double -> Double -> Double
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

punchOutCalCurveBCAD :: Int -> Int -> CalCurveBCAD -> CalCurveBCAD
punchOutCalCurveBCAD start stop (CalCurveBCAD cals uncals sigmas) =
    let startIndex = fromMaybe 0 $ VU.findIndex (>= start) cals
        stopIndex = VU.length cals - fromMaybe 0 (VU.findIndex (<= stop) $ VU.reverse cals)
        toIndex = stopIndex - startIndex
    --in error $ show $ (start, stop, VU.slice startIndex toIndex cals)
    in CalCurveBCAD
       (VU.slice startIndex toIndex cals)
       (VU.slice startIndex toIndex uncals)
       (VU.slice startIndex toIndex sigmas)

vectorBPToBCAD :: VU.Vector YearBP -> VU.Vector YearBCAD
vectorBPToBCAD = VU.map bp2BCAD

bp2BCAD :: YearBP -> YearBCAD
bp2BCAD x = -(fromIntegral x) + 1950

bcad2BP :: YearBCAD -> YearBP
bcad2BP y = 1950 - fromIntegral y

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
        getInBetweenPoints :: (Double, Double) -> (Double, Double) -> Double -> (Double, Double)
        getInBetweenPoints (x1,y1) (x2,y2) xPred =
            let yDiff = y2 - y1
                xDiff = abs $ x1 - x2
                yDiffPerxDiff = yDiff/xDiff
                xPredRel = x1 - xPred
            in (xPred, y1 + xPredRel * yDiffPerxDiff)
        uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
        uncurry3 f ~(a,b,c) = f a b c

trimLowDensityEdgesCalPDF :: CalPDF -> CalPDF
trimLowDensityEdgesCalPDF (CalPDF name cals dens) =
    let firstAboveThreshold = fromMaybe 0 (VU.findIndex (> 0.00001) dens)
        lastAboveThreshold = fromMaybe 0 (VU.findIndex (> 0.00001) $ VU.reverse dens)
        untilLastAboveThreshold = VU.length dens - firstAboveThreshold - lastAboveThreshold
        calsSlice = VU.slice firstAboveThreshold untilLastAboveThreshold cals
        densSlice = VU.slice firstAboveThreshold untilLastAboveThreshold dens
    in CalPDF name calsSlice densSlice
