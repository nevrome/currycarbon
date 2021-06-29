module Currycarbon.Calibration where

import Currycarbon.Parsers
import Currycarbon.Types
import Currycarbon.Utils

import Data.List (elemIndices, sort, genericLength, tails, nub) 

calibrateMany :: CalCurve -> [UncalC14] -> [CalPDF]
calibrateMany calCurve =
    map ((\(x,_,_) -> x) . calibrate calCurve)

calibrate :: CalCurve -> UncalC14 -> (CalPDF, CalCurve, CalCurveMatrix)
calibrate calCurve uncalDate =
    let -- prepare PDF for uncalibrated date
        uncalPDF = uncalToPDF uncalDate
        -- prepare relevant segment of the calcurve
        calCurveSegment = makeBCCalCurve $
                            interpolateCalCurve $
                            getRelevantCalCurveSegment uncalPDF calCurve
        -- transform calCurve to matrix
        calCurveMatrix = makeCalCurveMatrix calCurveSegment
        -- perform projection (aka calibration)
        calPDF = normalizeCalPDF $ projectUncalOverCalCurve uncalPDF calCurveMatrix
    in (calPDF,calCurveSegment,calCurveMatrix)

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
dnormInt mu sigma x =
    let muDouble = fromIntegral mu
        sigmaDouble = fromIntegral sigma
        xDouble = fromIntegral x
    in dnorm muDouble sigmaDouble xDouble
    where
        dnorm :: Double -> Double -> Double -> Double 
        dnorm mu sigma x = 
            let a = recip (sqrt (2 * pi * sigma2))
                b = exp ((-(realToFrac x - realToFrac mu)^2) / (2 * sigma2))
                sigma2 = realToFrac sigma^2
            in a * b

-- this is a relatively imprecise solution, because start and end of the range may be badly represented
getRelevantCalCurveSegment :: UncalPDF -> CalCurve -> CalCurve
getRelevantCalCurveSegment uncalPDF (CalCurve obs) = 
    let minSearchBP = minimum $ getBPsUncal uncalPDF
        maxSearchBP = maximum $ getBPsUncal uncalPDF
        (smallerMin,biggerMin) = splitWhen (\(x,_,_) -> x < minSearchBP) obs
        (smallerMax,biggerMax) = splitWhen (\(x,_,_) -> x <= maxSearchBP) biggerMin
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
timeWindows xs = map (\xs -> (head xs, last xs)) $ windows 2 xs
    where
        windows :: Int -> [a] -> [[a]]
        windows n xs = takeLengthOf (drop (n-1) xs) (windows' n xs)
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

makeCalCurveMatrix :: CalCurve -> CalCurveMatrix
makeCalCurveMatrix calCurve =
    let bps = getBPs calCurve
        cals = getCals calCurve
        sigmas = getCalSigmas calCurve
        bpsMatrix = [(minimum bps)..(maximum bps)]
        calsMatrix = [(minimum cals)..(maximum cals)]
    in CalCurveMatrix bpsMatrix calsMatrix $ map (\x -> map (fillCell calCurve x) bpsMatrix) calsMatrix
    where 
        fillCell :: CalCurve -> Int -> Int -> Double
        fillCell calCurve matrixPosCal matrixPosBP =
            let bps = getBPs calCurve
                cals = getCals calCurve
                sigmas = getCalSigmas calCurve
                relevantIndex = head $ elemIndices matrixPosCal cals
                mean = bps !! relevantIndex
                sigma = sigmas !! relevantIndex
            in dnormInt mean sigma matrixPosBP

projectUncalOverCalCurve :: UncalPDF -> CalCurveMatrix -> CalPDF
projectUncalOverCalCurve uncalPDF (CalCurveMatrix _ cal matrix) =
    CalPDF (getNameUncal uncalPDF) $ zip cal (matrixColSum $ vectorMatrixMult (getProbsUncal uncalPDF) matrix)
    where
        vectorMatrixMult :: [Double] -> [[Double]] -> [[Double]]
        vectorMatrixMult vec mat = map (\x -> zipWith (*) x vec) mat
        matrixColSum :: [[Double]] -> [Double]
        matrixColSum = map sum