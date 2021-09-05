module Currycarbon.Calibration
    ( -- * Calibration functions
      --
      -- $calibration
      --
      -- This module implements the actual calibration logic
        prepareCalCurve
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

-- | Take a raw calibration curve and an uncalibrated date and return
-- a tuple with the relevant segment of the calibration curve in standard-
-- and matrix-format
prepareCalCurve :: Bool -> CalCurve -> UncalPDF -> (CalCurve, CalCurveMatrix)
prepareCalCurve noInterpolate calCurve uncalPDF =
    let -- prepare relevant segment of the calcurve
        rawCalCurveSegment = getRelevantCalCurveSegment uncalPDF calCurve
        calCurveSegment = makeBCADCalCurve $
            if noInterpolate
            then rawCalCurveSegment
            else interpolateCalCurve rawCalCurveSegment
        -- transform calCurve to matrix
        calCurveMatrix = makeCalCurveMatrix uncalPDF calCurveSegment
    in (calCurveSegment,calCurveMatrix)

makeBCADCalCurve :: CalCurve -> CalCurve
makeBCADCalCurve (CalCurve bps cals sigmas) = CalCurve (VU.map (\a -> -a+1950) bps) (VU.map (\b -> -b+1950) cals) sigmas

interpolateCalCurve :: CalCurve -> CalCurve
interpolateCalCurve (CalCurve bps cals sigmas) = 
    let obs = VU.toList $ VU.zip3 bps cals sigmas
        obsFilled = concat $ map fillWindowsCal (timeWindows obs) ++ [[last obs]]
    in CalCurve (VU.fromList $ map (\(a,_,_) -> a) obsFilled) (VU.fromList $ map (\(_,b,_) -> b) obsFilled) (VU.fromList $ map (\(_,_,c) -> c) obsFilled)
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

-- this is a relatively imprecise solution, because start and end of the range may be badly represented
getRelevantCalCurveSegment :: UncalPDF -> CalCurve -> CalCurve
getRelevantCalCurveSegment (UncalPDF _ bps' _) (CalCurve bps cals sigmas) = 
    let obs = VU.toList $ VU.zip3 bps cals sigmas 
        start = VU.head bps'
        stop = VU.last bps'
        (afterStart,_) = splitWhen (\(x,_,_) -> x <= start) obs
        (_,beforeStop) = splitWhen (\(x,_,_) -> x <= stop) $ reverse afterStart
        finalList = reverse beforeStop
    in CalCurve (VU.fromList $ map (\(a,_,_) -> a) finalList) (VU.fromList $ map (\(_,b,_) -> b) finalList) (VU.fromList $ map (\(_,_,c) -> c) finalList)

splitWhen :: (a -> Bool) -> [a] -> ([a],[a])
splitWhen _ [] = ([],[])
splitWhen pre [x] = if pre x then ([x],[]) else ([],[x])
splitWhen pre (x:xs) = combine (splitWhen pre [x]) (splitWhen pre xs)
    where
        combine :: ([a],[a]) -> ([a],[a]) -> ([a],[a])
        combine (a1,b1) (a2,b2) = (a1++a2,b1++b2)

makeCalCurveMatrix :: UncalPDF -> CalCurve -> CalCurveMatrix
makeCalCurveMatrix uncalPDF (CalCurve bps cals sigmas) =
    let bps' = VU.reverse $ VU.map fromIntegral bps
        sigmas' = VU.reverse $ VU.map fromIntegral sigmas
        uncalbps = VU.map (\x -> negate x + 1950) (getBPsUncal uncalPDF)
        uncalbpsFloat = VU.map fromIntegral $ convert bps
    in CalCurveMatrix uncalbps cals $ buildMatrix bps' sigmas' uncalbpsFloat
    where
        buildMatrix :: VU.Vector Float -> VU.Vector Float -> VU.Vector Float -> V.Vector (VU.Vector Float)
        buildMatrix bps_ sigmas_ uncalbps_ = V.map (\x -> VU.map (fillCell x) uncalbps_) $ V.zip (convert bps_) (convert sigmas_)
        fillCell :: (Float, Float) -> Float -> Float
        fillCell (bp, sigma) matrixPosBP = 
            if abs (bp - matrixPosBP) < 5*sigma
            then dnorm bp sigma matrixPosBP
            else 0

-- | Transform an uncalibrated date to an uncalibrated 
-- probability density table
uncalToPDF :: UncalC14 -> UncalPDF
uncalToPDF (UncalC14 name mean std) =
    let meanFloat = fromIntegral mean
        stdFloat = fromIntegral std
        years = VU.reverse $ VU.enumFromN (mean-5*std) (5*std)
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

-- | Calibrates a list of dates with the provided calibration curve
calibrateDates :: Bool -> CalCurve -> [UncalC14] -> [CalPDF]
calibrateDates _ _ [] = []
calibrateDates interpolate calCurve uncalDates =
    map (calibrateDate interpolate calCurve) uncalDates `using` parList rpar

calibrateDate :: Bool -> CalCurve -> UncalC14 -> CalPDF
calibrateDate interpolate calCurve uncalDate =
    let -- prepare PDF for uncalibrated date
        uncalPDF = uncalToPDF uncalDate
        -- prepare calCurve
        (_,calCurveMatrix) = prepareCalCurve interpolate calCurve uncalPDF
        -- perform projection (aka calibration)
        calPDF = normalizeCalPDF $ projectUncalOverCalCurve uncalPDF calCurveMatrix
    in calPDF

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
refineCalDate (CalPDF name bps probs) =
    let sortedDensities = sortBy (flip (\ (_, dens1) (_, dens2) -> compare dens1 dens2)) (VU.toList $ VU.zip bps probs)
        cumsumDensities = scanl1 (+) $ map snd sortedDensities
        isIn68 = map (< 0.683) cumsumDensities
        isIn95 = map (< 0.954) cumsumDensities
        contextualizedDensities = reverse $ sort $ zipWith3 (\(year,dens) in68 in95 -> (year,dens,in68,in95)) sortedDensities isIn68 isIn95
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
