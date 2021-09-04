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
import Data.Foldable (foldl')

fastSum :: Num a => [a] -> a
fastSum = foldl' (+) 0

-- | Take a raw calibration curve and an uncalibrated date and return
-- a tuple with the relevant segment of the calibration curve in standard-
-- and matrix-format
prepareCalCurve :: Bool -> CalCurve -> UncalPDF -> (CalCurve, CalCurveMatrix)
prepareCalCurve interpolate calCurve uncalPDF =
    let -- prepare relevant segment of the calcurve
        rawCalCurveSegment = getRelevantCalCurveSegment uncalPDF calCurve
        calCurveSegment = makeBCADCalCurve $
            if interpolate
            then interpolateCalCurve rawCalCurveSegment
            else rawCalCurveSegment
        -- transform calCurve to matrix
        calCurveMatrix = makeCalCurveMatrix uncalPDF calCurveSegment
    in (calCurveSegment,calCurveMatrix)

makeBCADCalCurve :: CalCurve -> CalCurve
makeBCADCalCurve (CalCurve x) = CalCurve $ map (\(a,b,c) -> (-a+1950,-b+1950,c)) x

interpolateCalCurve :: CalCurve -> CalCurve
interpolateCalCurve (CalCurve obs) = 
    let obsFilled = concat $ map fillWindowsCal (timeWindows obs) ++ [[last obs]]
    in CalCurve obsFilled
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
getRelevantCalCurveSegment uncalPDF (CalCurve obs) = 
    let bps = getBPsUncal uncalPDF
        start = head bps
        stop = last bps
        (afterStart,_) = splitWhen (\(x,_,_) -> x <= start) obs
        (_,beforeStop) = splitWhen (\(x,_,_) -> x <= stop) $ reverse afterStart
    in CalCurve $ reverse beforeStop

splitWhen :: (a -> Bool) -> [a] -> ([a],[a])
splitWhen _ [] = ([],[])
splitWhen pre [x] = if pre x then ([x],[]) else ([],[x])
splitWhen pre (x:xs) = combine (splitWhen pre [x]) (splitWhen pre xs)
    where
        combine :: ([a],[a]) -> ([a],[a]) -> ([a],[a])
        combine (a1,b1) (a2,b2) = (a1++a2,b1++b2)

makeCalCurveMatrix :: UncalPDF -> CalCurve -> CalCurveMatrix
makeCalCurveMatrix uncalPDF (CalCurve obs) =
    let obsFloat = map (\(x,y,z) -> (fromIntegral x, fromIntegral y, fromIntegral z)) $ reverse obs
        bps = map (\x -> negate x + 1950) (getBPsUncal uncalPDF)
        bpsFloat = map fromIntegral bps
        cals = reverse $ getCals (CalCurve obs)
    in CalCurveMatrix bps cals $ buildMatrix obsFloat bpsFloat
    where
        buildMatrix :: [(Float, Float, Float)] -> [Float] -> [[Float]]
        buildMatrix obs bps = map (\x -> map (fillCell x) bps) obs
        fillCell :: (Float, Float, Float) -> Float -> Float
        fillCell (bp,_,sigma) matrixPosBP = dnorm bp sigma matrixPosBP

-- | Transform an uncalibrated date to an uncalibrated 
-- probability density table
uncalToPDF :: UncalC14 -> UncalPDF
uncalToPDF (UncalC14 name mean std) =
    let meanFloat = fromIntegral mean
        stdFloat = fromIntegral std
        years = reverse [(mean-5*std) .. (mean+5*std)]
        yearsFloat = map fromIntegral years
        probabilities = map (dnorm meanFloat stdFloat) yearsFloat
    in UncalPDF name $ zip years probabilities

dnorm :: Float -> Float -> Float -> Float 
dnorm mu sigma x = 
    let a = recip (sqrt (2 * pi * sigma2))
        b = exp ((-((x - mu)**2)) / (2 * sigma2))
        sigma2 = sigma**2
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
normalizeCalPDF calPDF = 
    let dens = getProbsCal calPDF
        sumDens = fastSum $ getProbsCal calPDF
        normalizedDens = map (/ sumDens) dens
    in CalPDF (_calPDFid calPDF) $ zip (getBPsCal calPDF) normalizedDens

projectUncalOverCalCurve :: UncalPDF -> CalCurveMatrix -> CalPDF
projectUncalOverCalCurve uncalPDF (CalCurveMatrix _ cal matrix) =
    let name = _uncalPDFid uncalPDF
        uncalDens = getProbsUncal uncalPDF
    in CalPDF name $ zip cal $ vectorMatrixMultSum uncalDens matrix
    where
        vectorMatrixMultSum :: [Float] -> [[Float]] -> [Float]
        vectorMatrixMultSum vec mat = map (\x -> fastSum $ zipWith (*) x vec) mat

-- | Transforms the raw, calibrated probability density table to a meaningful representation of a
-- calibrated radiocarbon date
refineCalDates :: [CalPDF] -> [CalC14]
refineCalDates = map refineCalDate

refineCalDate :: CalPDF -> CalC14
refineCalDate (CalPDF name densities) =
    let sortedDensities = sortBy (flip (\ (_, dens1) (_, dens2) -> compare dens1 dens2)) densities
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
