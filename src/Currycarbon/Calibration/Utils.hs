{-# LANGUAGE Strict #-}

module Currycarbon.Calibration.Utils where

import           Currycarbon.CalCurves (interpolateCalCurve)
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
    let std' = max std 10
        start = mean+6*std'
        stop = mean-6*std'
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

-- | Subset the calibration curve to the non-zero density range. The threshold is set to 0.00001.
trimLowDensityEdgesCalPDF :: CalPDF -> CalPDF
trimLowDensityEdgesCalPDF (CalPDF name cals dens) =
    let firstAboveThreshold = fromMaybe 0 (VU.findIndex (> 0.00001) dens)
        lastAboveThreshold = fromMaybe 0 (VU.findIndex (> 0.00001) $ VU.reverse dens)
        untilLastAboveThreshold = VU.length dens - firstAboveThreshold - lastAboveThreshold
        calsSlice = VU.slice firstAboveThreshold untilLastAboveThreshold cals
        densSlice = VU.slice firstAboveThreshold untilLastAboveThreshold dens
    in CalPDF name calsSlice densSlice
