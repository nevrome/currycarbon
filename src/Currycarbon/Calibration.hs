module Currycarbon.Calibration where

import Currycarbon.Parsers
import Currycarbon.Types
import Currycarbon.Utils

import Data.List (elemIndices, sort, genericLength) 

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
    let years = reverse [(mean-4*std) .. (mean+4*std)]
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

getRelevantCalCurveSegment :: UncalPDF -> CalCurve -> CalCurve
getRelevantCalCurveSegment uncalPDF (CalCurve obs) = 
    let minSearchBP = minimum $ getBPsUncal uncalPDF
        maxSearchBP = maximum $ getBPsUncal uncalPDF
        (smallerMin,biggerMin) = splitWhenSorted (\(x,_,_) -> x <= minSearchBP) obs
        (smallerMax,biggerMax) = splitWhenSorted (\(x,_,_) -> x < maxSearchBP) (last smallerMin : biggerMin)
    in CalCurve (smallerMax ++ [head biggerMax])

interpolateCalCurve :: CalCurve -> CalCurve
interpolateCalCurve calCurve = 
    let -- input observations
        bps = getBPs calCurve
        cals = getCals calCurve
        sigmas = getCalSigmas calCurve
        -- find and fill missing uncalBP years
        fillBPBPs = filter (`notElem` bps) [(minimum bps)..(maximum bps)]
        fillBPCals = map (curveInterpolInt bps cals) fillBPBPs
        fillBPCalSigmas = map (curveInterpolInt bps sigmas) fillBPBPs
        -- find and fill missing calBP years
        fillCalCals = filter (`notElem` cals ++ fillBPCals) [(minimum cals)..(maximum cals)]
        fillCalBPs = map (curveInterpolInt cals bps) fillCalCals
        fillCalCalSigmas = map (curveInterpolInt bps sigmas) fillCalBPs
    in -- merge observations and interpolated values
        CalCurve $ sort $
        zip3 bps cals sigmas ++
        zip3 fillBPBPs fillBPCals fillBPCalSigmas ++ 
        zip3 fillCalBPs fillCalCals fillCalCalSigmas
    where
        curveInterpolInt :: [Int] -> [Int] -> Int -> Int
        curveInterpolInt xs ys xPred =
            let xsDouble = map fromIntegral xs
                ysDouble = map fromIntegral ys
                xPredDouble = fromIntegral xPred
            in round $ curveInterpol xsDouble ysDouble xPredDouble
        curveInterpol :: [Double] -> [Double] -> Double -> Double
        curveInterpol xs ys xPred
            | xPred `elem` xs = 
                let values = map (ys !!) $ xPred `elemIndices` xs
                in sum values / genericLength values -- mean
            | otherwise =
                let (xsLeft,xsRight) = splitWhen (< xPred) xs
                    xLeft = maximum xsLeft
                    xRight = minimum xsRight
                    iLeft = head $ elemIndices xLeft xs
                    iRight = head $ elemIndices xRight xs
                in snd $ getInBetweenPoints (xs !! iLeft, ys !! iLeft) (xs !! iRight, ys !! iRight) xPred
        getInBetweenPoints :: (Double, Double) -> (Double, Double) -> Double -> (Double, Double)
        getInBetweenPoints (x1,y1) (x2,y2) xPred =
            let yDiff = y2 - y1
                xDiff = abs $ x1 - x2
                yDiffPerxDiff = yDiff/xDiff
                xPredRel = xPred - x1
            in (xPred, y1 + xPredRel * yDiffPerxDiff)

splitWhenSorted :: (Ord a) => (a -> Bool) -> [a] -> ([a],[a])
splitWhenSorted pre xs = sortTupleLists $ splitWhen pre xs
    where sortTupleLists (xs,ys) = (sort xs, sort ys)

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
                relevantIndex = middle $ elemIndices matrixPosCal cals
                mean = bps !! relevantIndex
                sigma = sigmas !! relevantIndex
            in dnormInt mean sigma matrixPosBP
        middle :: [a] -> a
        middle [x] = x
        middle xs = xs !! ((length xs `div` 2) - 1)

projectUncalOverCalCurve :: UncalPDF -> CalCurveMatrix -> CalPDF
projectUncalOverCalCurve uncalPDF (CalCurveMatrix _ cal matrix) =
    CalPDF (getNameUncal uncalPDF) $ zip cal (matrixColSum $ vectorMatrixMult (getProbsUncal uncalPDF) matrix)
    where
        vectorMatrixMult :: [Double] -> [[Double]] -> [[Double]]
        vectorMatrixMult vec mat = map (\x -> zipWith (*) x vec) mat
        matrixColSum :: [[Double]] -> [Double]
        matrixColSum = map sum