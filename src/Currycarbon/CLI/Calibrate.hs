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

data CalibrateOptions = CalibrateOptions {   
      _calibrateC14Age :: Int,
      _calibrateC14Std :: Int
    }

runCalibrate :: CalibrateOptions -> IO ()
runCalibrate (CalibrateOptions c14Age c14Std) = do
    calCurve <- readCalCurve
    let date = uncalToPDF $ UncalC14 c14Age c14Std
    print date
    let calCurveMatrix = createRelevantCalCurveMatrix calCurve date
    print calCurveMatrix
    let calPDF = projectUncalOverCalCurve calCurveMatrix date
    print calPDF      
    return ()

projectUncalOverCalCurve :: CalCurveMatrix -> UncalPDF -> CalPDF
projectUncalOverCalCurve (CalCurveMatrix _ cal matrix) (UncalPDF uncalBP probabilities) =
    CalPDF cal (matrixColSum $ vectorMatrixMult probabilities matrix)

createRelevantCalCurveMatrix :: CalCurve -> UncalPDF -> CalCurveMatrix
createRelevantCalCurveMatrix (CalCurve bp cal) (UncalPDF bpDate _) =
    let iInRange = findIndices (`elem` bp) bpDate
        filteredCalCurve = CalCurve (map (bp !!) iInRange) (map (cal !!) iInRange)
        completedCalCurve = completeCalCurve filteredCalCurve
    in makeCalCurveMatrix completedCalCurve

makeCalCurveMatrix :: CalCurve -> CalCurveMatrix
makeCalCurveMatrix (CalCurve bp cal) =
    let bpMatrix = [(minimum bp)..(maximum bp)]
        calMatrix = [(minimum cal)..(maximum cal)]
    in CalCurveMatrix bpMatrix calMatrix $ map (\x -> map (huhu (CalCurve bp cal) x) bpMatrix) calMatrix
    where 
        huhu :: CalCurve -> Int -> Int -> Double
        huhu (CalCurve bp cal) matrixPosBP matrixPosCal =
            let range = elemIndices matrixPosBP bp
            in if matrixPosCal `elem` map (cal !!) range
               then 1
               else 0

completeCalCurve :: CalCurve -> CalCurve
completeCalCurve (CalCurve bp cal) = 
    let newBP = [(minimum bp)..(maximum bp)]
        newCal = map (curveInterpolInt bp cal) newBP
    in CalCurve newBP newCal

curveInterpolInt :: [Int] -> [Int] -> Int -> Int
curveInterpolInt xs ys xPred = 
    let xsDouble = map fromIntegral xs
        ysDouble = map fromIntegral ys
        xPredDouble = fromIntegral xPred
    in round $ curveInterpol xsDouble ysDouble xPredDouble

curveInterpol :: [Double] -> [Double] -> Double -> Double
curveInterpol xs ys xPred =
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
    let years = reverse [(mean-std) .. (mean+std)]
        probabilities = map (dnormInt mean std) years
    in UncalPDF years probabilities

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

