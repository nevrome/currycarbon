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
                            fillCalInCalCurve $ 
                            getRelevantCalCurveSegment uncalPDF $ 
                            interpolateCalCurve calCurve
    -- perform projection (aka calibration)
    let calPDF = makeBCCalPDF $ 
                            projectUncalOverCalCurve uncalPDF $ 
                            makeCalCurveMatrix calCurveSegment
    -- plot calcurveSegmemt
    let maxBPCalCurve =     fromIntegral $ maximum $ getBPs calCurveSegment
        minBPCalCurve =     fromIntegral $ minimum $ getBPs calCurveSegment
        maxCalCalCurve =    fromIntegral $ maximum $ getCals calCurveSegment
        minCalCalCurve =    fromIntegral $ minimum $ getCals calCurveSegment
    printPlot $ 
        emptyXYPlot 
        `thenPlot` getCalCurveValue calCurveSegment 
        `xlim` (maxCalCalCurve, minCalCalCurve) 
        `ylim` (minBPCalCurve, maxBPCalCurve)
    -- plot calPDF
    let maxBPCalPDF =       fromIntegral $ maximum $ getBPsCal calPDF
        minBPCalPDF =       fromIntegral $ minimum $ getBPsCal calPDF
    printPlot $ 
        emptyXYPlot 
        `thenPlot` getCalPDFValue calPDF 
        `xlim` (maxBPCalPDF, minBPCalPDF) 
        `ylim` (0 , maximum $ getProbsCal calPDF)

makeBCCalPDF :: CalPDF -> CalPDF
makeBCCalPDF calPDF = CalPDF $ zip (map (\x -> x - 1950) $ getBPsCal calPDF) (getProbsCal calPDF)

makeBCCalCurve :: CalCurve -> CalCurve
makeBCCalCurve calCurve = CalCurve $ zip (getBPs calCurve) (map (\x -> x - 1950) $ getCals calCurve)

getCalCurveValue :: CalCurve -> Function
getCalCurveValue (CalCurve obs) x
  = fromIntegral $ fst $ head $ filter (\(_, y) -> round x == y) obs

getCalPDFValue :: CalPDF -> Function
getCalPDFValue (CalPDF obs) x
  = snd $ head $ filter (\(y, _) -> round x == y) obs

projectUncalOverCalCurve :: UncalPDF -> CalCurveMatrix -> CalPDF
projectUncalOverCalCurve uncalPDF (CalCurveMatrix _ cal matrix) =
    CalPDF $ zip cal (matrixColSum $ vectorMatrixMult (getProbsUncal uncalPDF) matrix)

getRelevantCalCurveSegment :: UncalPDF -> CalCurve -> CalCurve
getRelevantCalCurveSegment uncalPDF calCurve =
    filterCalCurve (\(x,_) -> x `elem` getBPsUncal uncalPDF) calCurve

filterCalCurve :: ((Int,Int) -> Bool) -> CalCurve -> CalCurve 
filterCalCurve pre (CalCurve curve) = CalCurve $ filter pre curve

getBPsCal :: CalPDF -> [Int]
getBPsCal (CalPDF obs) = map fst obs

getProbsCal :: CalPDF -> [Double]
getProbsCal (CalPDF obs) = map snd obs

getBPsUncal :: UncalPDF -> [Int]
getBPsUncal (UncalPDF obs) = map fst obs

getProbsUncal :: UncalPDF -> [Double]
getProbsUncal (UncalPDF obs) = map snd obs

getBPs :: CalCurve -> [Int]
getBPs (CalCurve obs) = map fst obs

getCals :: CalCurve -> [Int]
getCals (CalCurve obs) = map snd obs

makeCalCurveMatrix :: CalCurve -> CalCurveMatrix
makeCalCurveMatrix calCurve =
    let bps = getBPs calCurve
        cals = getCals calCurve
        bpsMatrix = [(minimum bps)..(maximum bps)]
        calsMatrix = [(minimum cals)..(maximum cals)]
    in CalCurveMatrix bpsMatrix calsMatrix $ map (\x -> map (fillCell calCurve x) bpsMatrix) calsMatrix
    where 
        fillCell :: CalCurve -> Int -> Int -> Double
        fillCell (CalCurve curve) matrixPosBP matrixPosCal =
            if (matrixPosCal,matrixPosBP) `elem` curve -- why is it this way round?
            then 1
            else 0

interpolateCalCurve :: CalCurve -> CalCurve
interpolateCalCurve calCurve = 
    let bps = getBPs calCurve
        cals = getCals calCurve
        newBP = [(minimum bps)..(maximum bps)]
        newCal = map (curveInterpolInt bps cals) newBP
    in CalCurve $ zip newBP newCal

fillCalInCalCurve :: CalCurve -> CalCurve
fillCalInCalCurve (CalCurve obs) =
    let minCalCalCurve = minimum $ map snd obs
        maxCalCalCurve = maximum $ map snd obs
        missing = filter (\x -> x `notElem` map snd obs) [minCalCalCurve..maxCalCalCurve]
    in CalCurve $ recursiveFill obs missing
    where 
        recursiveFill :: [(Int, Int)] -> [Int] -> [(Int, Int)]
        recursiveFill obs [] = obs
        recursiveFill obs [x] = completeList obs x
        recursiveFill obs (x:xs) = recursiveFill (completeList obs x) xs
        completeList :: [(Int, Int)] -> Int -> [(Int, Int)]
        completeList obs insert =
            let (beforeInsert, afterInsert) = splitWhen (\(_,x) -> x < insert) obs
            in beforeInsert ++ [(fst $ last beforeInsert, insert)] ++ afterInsert

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

splitWhen :: (a -> Bool) -> [a] -> ([a],[a])
splitWhen _ [] = ([],[])
splitWhen pre [x] = if pre x then ([x],[]) else ([],[x])
splitWhen pre (x:xs) = combine (splitWhen pre [x]) (splitWhen pre xs)
    where
        combine :: ([a],[a]) -> ([a],[a]) -> ([a],[a])
        combine (a1,b1) (a2,b2) = (a1++a2,b1++b2)

getInBetweenPoints :: (Double, Double) -> (Double, Double) -> Double -> (Double, Double)
getInBetweenPoints (x1,y1) (x2,y2) xPred =
    let yDiff = y2 - y1
        xDiff = abs $ x1 - x2
        yDiffPerxDiff = yDiff/xDiff
        xPredRel = xPred - x1
    in (xPred, y1 + xPredRel * yDiffPerxDiff)

matrixColSum :: [[Double]] -> [Double]
matrixColSum = map sum

vectorMatrixMult :: [Double] -> [[Double]] -> [[Double]]
vectorMatrixMult vec mat = map (\x -> zipWith (*) x vec) mat

uncalToPDF :: UncalC14 -> UncalPDF
uncalToPDF (UncalC14 mean std) = 
    let years = reverse [(mean-3*std) .. (mean+3*std)]
        probabilities = map (dnormInt mean std) years
    in UncalPDF $ zip years probabilities

dnormInt :: Int -> Int -> Int -> Double
dnormInt mu sigma x =
    let muDouble = fromIntegral mu
        sigmaDouble = fromIntegral sigma
        xDouble = fromIntegral x
    in dnorm muDouble sigmaDouble xDouble

dnorm :: Double -> Double -> Double -> Double 
dnorm mu sigma x = a * b
  where
    a = recip (sqrt (2 * pi * sigma2))
    b = exp ((-(realToFrac x - realToFrac mu)^2) / (2 * sigma2))
    sigma2 = realToFrac sigma^2

