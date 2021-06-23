module Currycarbon.CLI.Calibrate where

import           Currycarbon.Parsers
import           Currycarbon.Types
import           Currycarbon.Utils

import           Control.Exception              (catch, throwIO, Exception)
import           Control.Monad                  (forM, guard)
import           Data.List                      (nub, tails, sortBy, intersect, maximumBy, group, sort, intercalate, elemIndex, elemIndices, unfoldr, findIndices)
import           Data.Maybe                     (isJust, fromMaybe, catMaybes, fromJust)
import           System.FilePath                ((<.>), (</>))
import           System.IO                      (hPutStrLn, stderr, hPutStr)
import           TextPlot

data CalibrateOptions = CalibrateOptions {   
      _calibrateC14Age :: Int,
      _calibrateC14Std :: Int
    }

runCalibrate :: CalibrateOptions -> IO ()
runCalibrate (CalibrateOptions c14Age c14Std) = do
    -- prepare date
    let uncalDate = UncalC14 c14Age c14Std
        uncalPDF = uncalToPDF uncalDate
    -- prepare relevant segment of the calcurve
    calCurve <- readCalCurve
    let calCurveSegment = makeBCCalCurve $
                            interpolateCalCurve $
                            getRelevantCalCurveSegment uncalPDF calCurve
    -- perform projection (aka calibration)
    let calPDF = projectUncalOverCalCurve uncalPDF $
                            makeCalCurveMatrix calCurveSegment
    -- plots
    plotCalCurveSegment calCurveSegment
    plotCalPDF calPDF
    return ()

uncalToPDF :: UncalC14 -> UncalPDF
uncalToPDF (UncalC14 mean std) = 
    let years = reverse [(mean-4*std) .. (mean+4*std)]
        probabilities = map (dnormInt mean std) years
    in UncalPDF $ zip years probabilities

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
            | xPred `elem` xs = ys !! head (xPred `elemIndices` xs)
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
makeBCCalPDF calPDF = CalPDF $ zip (map (\x -> x - 1950) $ getBPsCal calPDF) (getProbsCal calPDF)

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
    CalPDF $ zip cal (matrixColSum $ vectorMatrixMult (getProbsUncal uncalPDF) matrix)
    where
        vectorMatrixMult :: [Double] -> [[Double]] -> [[Double]]
        vectorMatrixMult vec mat = map (\x -> zipWith (*) x vec) mat
        matrixColSum :: [[Double]] -> [Double]
        matrixColSum = map sum

plotCalCurveSegment :: CalCurve -> IO ()
plotCalCurveSegment calCurveSegment = do
    let maxBPCalCurve =     fromIntegral $ maximum $ getBPs calCurveSegment
        minBPCalCurve =     fromIntegral $ minimum $ getBPs calCurveSegment
        maxCalCalCurve =    fromIntegral $ maximum $ getCals calCurveSegment
        minCalCalCurve =    fromIntegral $ minimum $ getCals calCurveSegment
    printPlot $ 
        emptyXYPlot 
        `thenPlot` getCalCurveValue calCurveSegment 
        `xlim` (maxCalCalCurve, minCalCalCurve) 
        `ylim` (minBPCalCurve, maxBPCalCurve)
    where
        getCalCurveValue :: CalCurve -> Function
        getCalCurveValue calCurve x = 
            fromIntegral $ fst $ head $ filter (\(_, y) -> round x == y) $ zip (getBPs calCurve) (getCals calCurve)

plotCalPDF :: CalPDF -> IO ()
plotCalPDF calPDF = do
    let maxBPCalPDF =       fromIntegral $ maximum $ getBPsCal calPDF
        minBPCalPDF =       fromIntegral $ minimum $ getBPsCal calPDF
    printPlot $ 
        emptyXYPlot 
        `thenPlot` getCalPDFValue calPDF 
        `xlim` (maxBPCalPDF, minBPCalPDF) 
        `ylim` (0 , maximum $ getProbsCal calPDF)
    where
        getCalPDFValue :: CalPDF -> Function
        getCalPDFValue (CalPDF obs) x = 
            snd $ head $ filter (\(y,_) -> round x == y) obs