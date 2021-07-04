module Currycarbon.Calibration where

import Currycarbon.Types

import Control.Parallel.Strategies ( parList, rdeepseq, using)
import Data.List (sort, tails, sortBy, groupBy, elemIndex) 
import Data.Maybe (fromJust)

refineCal :: [CalPDF] -> [CalC14]
refineCal = map refineCalOne

refineCalOne :: CalPDF -> CalC14
refineCalOne (CalPDF name densities) =
    let sortedDensities = sortBy (flip (\ (_, dens1) (_, dens2) -> compare dens1 dens2)) densities
        cumsumDensities = scanl1 (+) $ map snd sortedDensities
        isIn68 = map (< 0.683) cumsumDensities
        isIn95 = map (< 0.954) cumsumDensities
        contextualizedDensities = reverse $ sort $ zipWith3 (\(year,dens) in68 in95 -> (year,dens,in68,in95)) sortedDensities isIn68 isIn95
    in CalC14 name (densities2HDR68 contextualizedDensities) (densities2HDR95 contextualizedDensities)
    where
        densities2HDR68 :: [(Int, Double, Bool, Bool)] -> [HDR]
        densities2HDR68 cDensities = 
            let highDensityGroups = groupBy (\(_,_,in681,_) (_,_,in682,_) -> in681 == in682) cDensities
                filteredDensityGroups = filter (all getIn68) highDensityGroups
            in map (\xs -> let yearRange = map getYear xs in HDR (head yearRange) (last yearRange)) filteredDensityGroups
        densities2HDR95 :: [(Int, Double, Bool, Bool)] -> [HDR]
        densities2HDR95 cDensities = 
            let highDensityGroups = groupBy (\(_,_,_,in951) (_,_,_,in952) -> in951 == in952) cDensities
                filteredDensityGroups = filter (all getIn95) highDensityGroups
            in map (\xs -> let yearRange = map getYear xs in HDR (head yearRange) (last yearRange)) filteredDensityGroups
        getIn68 :: (Int, Double, Bool, Bool) -> Bool
        getIn68 (_,_,x,_) = x
        getIn95 :: (Int, Double, Bool, Bool) -> Bool
        getIn95 (_,_,_,x) = x
        getYear :: (Int, Double, Bool, Bool) -> Int
        getYear (year,_,_,_) = year

calibrateMany :: CalCurve -> [UncalC14] -> [CalPDF]
calibrateMany calCurve uncalDates =
    let calDates = map (calibrate calCurve) uncalDates
    in calDates `using` parList rdeepseq

calibrate :: CalCurve -> UncalC14 -> CalPDF
calibrate calCurve uncalDate =
    let -- prepare PDF for uncalibrated date
        uncalPDF = uncalToPDF uncalDate
        -- prepare calCurve
        (_,calCurveMatrix) = prepareCalCurve calCurve uncalPDF
        -- perform projection (aka calibration)
        calPDF = normalizeCalPDF $ projectUncalOverCalCurve uncalPDF calCurveMatrix
    in calPDF

prepareCalCurve :: CalCurve -> UncalPDF -> (CalCurve, CalCurveMatrix)
prepareCalCurve calCurve uncalPDF =
    let -- prepare relevant segment of the calcurve
        calCurveSegment = makeBCCalCurve $
                            interpolateCalCurve $
                            getRelevantCalCurveSegment uncalPDF calCurve
        -- transform calCurve to matrix
        calCurveMatrix = makeCalCurveMatrix calCurveSegment
    in (calCurveSegment,calCurveMatrix)

normalizeCalPDF :: CalPDF -> CalPDF
normalizeCalPDF calPDF = 
    let dens = getProbsCal calPDF
        sumDens = sum $ getProbsCal calPDF
        normalizedDens = map (/ sumDens) dens
    in CalPDF (getNameCal calPDF) $ zip (getBPsCal calPDF) normalizedDens

uncalToPDF :: UncalC14 -> UncalPDF
uncalToPDF (UncalC14 name mean std) =
    let years = reverse [(mean-5*std) .. (mean+5*std)]
        probabilities = map (dnormInt mean std) years
    in UncalPDF name $ zip years probabilities

dnormInt :: Int -> Int -> Int -> Double
dnormInt muInt sigmaInt xInt =
    let muDouble = fromIntegral muInt
        sigmaDouble = fromIntegral sigmaInt
        xDouble = fromIntegral xInt
    in dnorm muDouble sigmaDouble xDouble
    where
        dnorm :: Double -> Double -> Double -> Double 
        dnorm mu sigma x = 
            let a = recip (sqrt (2 * pi * sigma2))
                b = exp ((-((x - mu)**2)) / (2 * sigma2))
                sigma2 = sigma**2
                dens = a*b
            -- only consider reasonably high densities (similar to Bchron)
            in if dens < 0.00001 then 0 else dens

-- this is a relatively imprecise solution, because start and end of the range may be badly represented
getRelevantCalCurveSegment :: UncalPDF -> CalCurve -> CalCurve
getRelevantCalCurveSegment uncalPDF (CalCurve obs) = 
    let bps = getBPsUncal uncalPDF
        minSearchBP = minimum bps
        maxSearchBP = maximum bps
        (_,biggerMin) = splitWhen (\(x,_,_) -> x < minSearchBP) obs
        (smallerMax,_) = splitWhen (\(x,_,_) -> x <= maxSearchBP) biggerMin
    in CalCurve smallerMax

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
        getInBetweenPoints :: (Double, Double) -> (Double, Double) -> Double -> (Double, Double)
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
        takeLengthOf = zipWith (flip const)
        windows' :: Int -> [a] -> [[a]]
        windows' n = map (take n) . tails

splitWhen :: (a -> Bool) -> [a] -> ([a],[a])
splitWhen _ [] = ([],[])
splitWhen pre [x] = if pre x then ([x],[]) else ([],[x])
splitWhen pre (x:xs) = combine (splitWhen pre [x]) (splitWhen pre xs)
    where
        combine :: ([a],[a]) -> ([a],[a]) -> ([a],[a])
        combine (a1,b1) (a2,b2) = (a1++a2,b1++b2)

makeBCCalCurve :: CalCurve -> CalCurve
makeBCCalCurve calCurve = CalCurve $ zip3 (getBPs calCurve) (map (\x -> x - 1950) $ getCals calCurve) (getCalSigmas calCurve)

makeBCCalPDF :: CalPDF -> CalPDF
makeBCCalPDF calPDF = CalPDF (getNameCal calPDF) $ zip (map (\x -> x - 1950) $ getBPsCal calPDF) (getProbsCal calPDF)

-- this only works, because the calCurve list is naturally ordered by the unique calibrated ages
-- if this is not the case, then the more complicated implementation from before V 0.3.2 is necessary
makeCalCurveMatrix :: CalCurve -> CalCurveMatrix
makeCalCurveMatrix (CalCurve obs) =
    let bps = getBPs (CalCurve obs)
        bpsMatrix = [(minimum bps)..(maximum bps)]
        cals = getCals (CalCurve obs)
    in CalCurveMatrix bpsMatrix (reverse cals) $ map (\x -> map (fillCell x) bpsMatrix) (reverse obs)
    where 
        fillCell :: (Int, Int, Int) -> Int -> Double
        fillCell (bp,_,sigma) matrixPosBP =
            dnormInt bp sigma matrixPosBP

projectUncalOverCalCurve :: UncalPDF -> CalCurveMatrix -> CalPDF
projectUncalOverCalCurve uncalPDF (CalCurveMatrix _ cal matrix) =
    CalPDF (getNameUncal uncalPDF) $ zip cal (matrixColSum $ vectorMatrixMult (getProbsUncal uncalPDF) matrix)
    where
        vectorMatrixMult :: [Double] -> [[Double]] -> [[Double]]
        vectorMatrixMult vec mat = map (\x -> zipWith (*) x vec) mat
        matrixColSum :: [[Double]] -> [Double]
        matrixColSum = map sum