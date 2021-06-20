module Currycarbon.CLI.Calibrate where

import           Currycarbon.Parsers
import           Currycarbon.Types
import           Currycarbon.Utils

import           Control.Exception              (catch, throwIO, Exception)
import           Control.Monad                  (forM, guard)
import           Data.List                      (nub, tails, sortBy, intersect, maximumBy, group, sort, intercalate, elemIndex, elemIndices, unfoldr)
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
    hPutStrLn stderr $ show $ getInBetweenPoints (0,0) (10,-20) 9
    -- hPutStrLn stderr $ show $ calCurveMatrix
    -- hPutStrLn stderr $ show $ calCurve
    -- hPutStrLn stderr $ show $ uncalToPDF $ UncalC14 2000 50
    -- hPutStrLn stderr $ show $ projectUncalOverCalCurve calCurveMatrix (uncalToPDF $ UncalC14 2000 50) 
    return ()

projectUncalOverCalCurve :: CalCurveMatrix -> UncalPDF -> CalPDF
projectUncalOverCalCurve (CalCurveMatrix matrix) (UncalPDF years probabilities) =
    CalPDF years (matrixColSum $ vectorMatrixMult probabilities matrix)

completeCalCurveMatrix :: CalCurveMatrix -> CalCurveMatrix
completeCalCurveMatrix mat = undefined 

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

--calCurveMatrix :: UncalPDF -> Matrix
calCurveMatrix :: CalCurveMatrix
calCurveMatrix = 
    CalCurveMatrix [[1,0,0,0,0], [0,0,0,0,0], [0,1,0,0,0], [0,0,0,0,0], [0,0,0,0,1]]

uncalToPDF :: UncalC14 -> UncalPDF
uncalToPDF (UncalC14 mean std) = 
    let years = reverse [(mean-std) .. (mean+std)]
        probabilities = map (dnorm mean std) years 
    in UncalPDF years probabilities

dnorm :: Double -> Double -> Double -> Double 
dnorm mu sigma x = a * b
  where
    a = recip (sqrt (2 * pi * sigma2))
    b = exp ((-(realToFrac x - realToFrac mu)^2) / (2 * sigma2))
    sigma2 = realToFrac sigma^2

