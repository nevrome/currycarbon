{-# LANGUAGE Strict #-}

module Currycarbon.Calibration
    ( -- * Calibration functions
      --
      -- $calibration
      --
      -- This module implements the actual calibration logic
        getRelevantCalCurveSegment
      , prepareCalCurveSegment
      , makeCalCurveMatrix
      , uncalToPDF
      , calibrateDates
      , refineCalDates
    ) where

import Currycarbon.Types

import Control.Parallel.Strategies (parList, using, rpar)
import Data.List (sort, tails, sortBy, groupBy)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import Data.Vector.Generic (convert)
import Data.Maybe (fromMaybe)

-- | Calibrates a list of dates with the provided calibration curve
calibrateDates :: CalibrationMethod -- ^ Calibration method
                  -> Bool -- ^ Should the calibration curve be interpolated for year-wise output?
                  -> CalCurve -- ^ Calibration curve
                  -> [UncalC14] -- ^ A list of uncalibrated radiocarbon dates
                  -> [CalPDF]
calibrateDates _ _ _ [] = []
calibrateDates MatrixMultiplication interpolate calCurve uncalDates =
    map (calibrateDateMatrixMult interpolate calCurve) uncalDates `using` parList rpar
calibrateDates Bchron interpolate calCurve uncalDates =
    map (calibrateDateBchron interpolate calCurve) uncalDates `using` parList rpar

calibrateDateMatrixMult :: Bool -> CalCurve -> UncalC14 -> CalPDF
calibrateDateMatrixMult interpolate calCurve uncalC14 =
    let rawCalCurveSegment = getRelevantCalCurveSegment uncalC14 calCurve
        calCurveSegment = prepareCalCurveSegment interpolate True rawCalCurveSegment
        uncalPDF = uncalToPDF uncalC14
        calCurveMatrix = makeCalCurveMatrix uncalPDF calCurveSegment
        calPDF = projectUncalOverCalCurve uncalPDF calCurveMatrix
    in normalizeCalPDF calPDF

calibrateDateBchron :: Bool -> CalCurve -> UncalC14 -> CalPDF
calibrateDateBchron interpolate calCurve uncalC14@(UncalC14 name age ageSd) =
    let rawCalCurveSegment = getRelevantCalCurveSegment uncalC14 calCurve
        CalCurve cals mus tau1s = prepareCalCurveSegment interpolate True rawCalCurveSegment
        ageFloat = -(fromIntegral age)+1950
        ageSd2 = ageSd*ageSd
        ageSd2Float = fromIntegral ageSd2
        musFloat = VU.map fromIntegral mus
        tau1sFloat = VU.map fromIntegral tau1s
        dens = VU.zipWith (\mu tau1 -> dnorm 0 1 ((ageFloat - mu) / sqrt (ageSd2Float + tau1 * tau1))) musFloat tau1sFloat
    in normalizeCalPDF $ CalPDF name cals dens

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

-- | Construct a matrix representation of a calibration curve for a given date
makeCalCurveMatrix :: UncalPDF -> CalCurve -> CalCurveMatrix
makeCalCurveMatrix (UncalPDF _ bps' _) (CalCurve cals bps sigmas) =
    let bpsFloat = VU.map fromIntegral bps
        sigmasFloat = VU.map fromIntegral sigmas
        uncalbps = VU.map (\x -> -x+1950) bps'
        uncalbpsFloat = VU.map fromIntegral uncalbps
    in CalCurveMatrix uncalbps cals $ buildMatrix bpsFloat sigmasFloat uncalbpsFloat
    where
        buildMatrix :: VU.Vector Float -> VU.Vector Float -> VU.Vector Float -> V.Vector (VU.Vector Float)
        buildMatrix bps_ sigmas_ uncalbps_ = V.map (\x -> VU.map (fillCell x) uncalbps_) $ V.zip (convert bps_) (convert sigmas_)
        fillCell :: (Float, Float) -> Float -> Float
        fillCell (bp, sigma) matrixPosBP = 
            if abs (bp - matrixPosBP) < 6*sigma
            then dnorm bp sigma matrixPosBP
            else 0

-- | Transform an uncalibrated date to an uncalibrated 
-- probability density table
uncalToPDF :: UncalC14 -> UncalPDF
uncalToPDF (UncalC14 name mean std) =
    let meanFloat = fromIntegral mean
        stdFloat = fromIntegral std
        years = VU.reverse $ VU.fromList [(mean-5*std) .. (mean+5*std)]
        yearsFloat = VU.map fromIntegral years
        probabilities = VU.map (dnorm meanFloat stdFloat) yearsFloat
    in UncalPDF name years probabilities

dnorm :: Float -> Float -> Float -> Float 
dnorm mu sigma x = 
    let a = recip (sqrt (2 * pi * sigma2))
        b = exp (-c2 / (2 * sigma2))
        c = x - mu
        c2 = c * c
        sigma2 = sigma * sigma
    in a*b

normalizeCalPDF :: CalPDF -> CalPDF
normalizeCalPDF (CalPDF name cals dens) = 
    let sumDens = VU.sum dens
        normalizedDens = VU.map (/ sumDens) dens
    in CalPDF name cals normalizedDens

projectUncalOverCalCurve :: UncalPDF -> CalCurveMatrix -> CalPDF
projectUncalOverCalCurve (UncalPDF name _ dens) (CalCurveMatrix _ cals matrix) =
    CalPDF name cals $ vectorMatrixMultSum dens matrix
    where
        vectorMatrixMultSum :: VU.Vector Float -> V.Vector (VU.Vector Float) -> VU.Vector Float
        vectorMatrixMultSum vec mat = convert $ V.map (\x -> VU.sum $ VU.zipWith (*) x vec) mat

-- | Transforms the raw, calibrated probability density table to a meaningful representation of a
-- calibrated radiocarbon date
refineCalDates :: [CalPDF] -> [CalC14]
refineCalDates = map refineCalDate

refineCalDate :: CalPDF -> CalC14
refineCalDate (CalPDF name bps dens) =
    let sortedDensities = sortBy (flip (\ (_, dens1) (_, dens2) -> compare dens1 dens2)) (VU.toList $ VU.zip bps dens)
        cumsumDensities = scanl1 (+) $ map snd sortedDensities
        isIn68 = map (< 0.683) cumsumDensities
        isIn95 = map (< 0.954) cumsumDensities
        contextualizedDensities = reverse $ sort $ zipWith3 (\(year,density) in68 in95 -> (year,density,in68,in95)) sortedDensities isIn68 isIn95
    in CalC14 name (densities2HDR68 contextualizedDensities) (densities2HDR95 contextualizedDensities)
    where
        densities2HDR68 :: [(Int, Float, Bool, Bool)] -> [HDR]
        densities2HDR68 cDensities = 
            let highDensityGroups = groupBy (\(_,_,in681,_) (_,_,in682,_) -> in681 == in682) cDensities
                filteredDensityGroups = filter (all getIn68) highDensityGroups
            in map (\xs -> let yearRange = map getYear xs in HDR (head yearRange) (last yearRange)) filteredDensityGroups
        densities2HDR95 :: [(Int, Float, Bool, Bool)] -> [HDR]
        densities2HDR95 cDensities = 
            let highDensityGroups = groupBy (\(_,_,_,in951) (_,_,_,in952) -> in951 == in952) cDensities
                filteredDensityGroups = filter (all getIn95) highDensityGroups
            in map (\xs -> let yearRange = map getYear xs in HDR (head yearRange) (last yearRange)) filteredDensityGroups
        getIn68 :: (Int, Float, Bool, Bool) -> Bool
        getIn68 (_,_,x,_) = x
        getIn95 :: (Int, Float, Bool, Bool) -> Bool
        getIn95 (_,_,_,x) = x
        getYear :: (Int, Float, Bool, Bool) -> Int
        getYear (year,_,_,_) = year
