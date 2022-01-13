{-# LANGUAGE Strict #-}

module Currycarbon.Calibration.Utils where

import Currycarbon.Types

import Data.List (tails)
import qualified Data.Vector.Unboxed as VU
import Data.Maybe (fromMaybe)
import Statistics.Distribution (density)
import Statistics.Distribution.StudentT (studentT)

dnorm :: Float -> Float -> Float -> Float 
dnorm mu sigma x = 
    let a = recip (sqrt (2 * pi * sigma2))
        b = exp (-c2 / (2 * sigma2))
        c = x - mu
        c2 = c * c
        sigma2 = sigma * sigma
    in a*b
    -- alternative implemenation with the statistics package: 
    -- realToFrac $ density (normalDistr (realToFrac mu) (realToFrac sigma)) (realToFrac x)

dt :: Double -> Float -> Float
dt dof x = realToFrac $ density (studentT (realToFrac dof)) (realToFrac x) -- dof: number of degrees of freedom

isOutsideRangeOfCalCurve :: CalCurve -> UncalC14 -> Bool
isOutsideRangeOfCalCurve (CalCurve _ bps _) (UncalC14 _ age _) = 
    age < VU.minimum bps || age > VU.maximum bps

-- | Take an uncalibrated date and a raw calibration curve and return
-- the relevant segment of the calibration curve
getRelevantCalCurveSegment :: UncalC14 -> CalCurve -> CalCurve
getRelevantCalCurveSegment (UncalC14 _ mean std) (CalCurve cals bps sigmas) =
    let start = mean+6*std
        stop = mean-6*std
        startIndex = fromMaybe 0 $ VU.findIndex (<= start) bps
        stopIndex = (VU.length bps - 1) - fromMaybe 0 (VU.findIndex (>= stop) $ VU.reverse bps)
        toIndex = stopIndex - startIndex
    in CalCurve (VU.slice startIndex toIndex cals) (VU.slice startIndex toIndex bps) (VU.slice startIndex toIndex sigmas)

-- | Modify a calibration curve (segment) with multiple optional steps, 
-- including interpolation and transforming dates to BC/AD format
prepareCalCurveSegment :: Bool -> Bool -> CalCurve -> CalCurve
prepareCalCurveSegment interpolate makeBCAD calCurve0 =
    let calCurve1 = if interpolate then interpolateCalCurve calCurve0 else calCurve0
        calCurve2 = if makeBCAD then makeBCADCalCurve calCurve1 else calCurve1
    in calCurve2

makeBCADCalCurve :: CalCurve -> CalCurve
makeBCADCalCurve (CalCurve cals bps sigmas) = CalCurve (VU.map (\b -> -b+1950) cals) (VU.map (\a -> -a+1950) bps) sigmas

interpolateCalCurve :: CalCurve -> CalCurve
interpolateCalCurve (CalCurve cals bps sigmas) =
    let obs = VU.toList $ VU.zip3 bps cals sigmas
        obsFilled = concat $ map fillWindowsCal (timeWindows obs) ++ [[last obs]]
    in CalCurve (VU.fromList $ map (\(_,b,_) -> b) obsFilled) (VU.fromList $ map (\(a,_,_) -> a) obsFilled) (VU.fromList $ map (\(_,_,c) -> c) obsFilled)
    where
        fillWindowsCal :: ((Int,Int,Int),(Int,Int,Int)) -> [(Int,Int,Int)]
        fillWindowsCal ((bp1,calbp1,sigma1),(bp2,calbp2,sigma2)) =
            if calbp1 == calbp2 || calbp1+1 == calbp2 || calbp1-1 == calbp2 
            then [(bp1,calbp1,sigma1)]
            else 
                let newCals = [calbp1,calbp1-1..calbp2+1]
                    newBPs = map (snd . getInBetweenPointsInt (calbp1,bp1) (calbp2,bp2)) newCals
                    newSigmas = map (snd . getInBetweenPointsInt (calbp1,sigma1) (calbp2,sigma2)) newCals
                in zip3 newBPs newCals newSigmas
        getInBetweenPointsInt :: (Int, Int) -> (Int, Int) -> Int -> (Int, Int)
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
        timeWindows :: [(a,b,c)] -> [((a,b,c),(a,b,c))]
        timeWindows xs = map (\ts -> (head ts, last ts)) $ windows 2 xs
            where
                windows :: Int -> [a] -> [[a]]
                windows n ys = takeLengthOf (drop (n-1) ys) (windows' n ys)
                takeLengthOf :: [a] -> [b] -> [b]
                takeLengthOf = zipWith (\_ x -> x)
                windows' :: Int -> [a] -> [[a]]
                windows' n = map (take n) . tails

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