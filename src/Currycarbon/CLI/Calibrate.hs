module Currycarbon.CLI.Calibrate where

import           Currycarbon.Parsers
import           Currycarbon.Types
import           Currycarbon.Utils

import           Control.Exception              (catch, throwIO, Exception)
import           Control.Monad                  (forM, guard)
import           Data.List                      (nub, tails, sortBy, intersect, maximumBy, group, sort, intercalate, elemIndex, elemIndices, unfoldr)
import           Data.Matrix                    as M
import           Data.Maybe                     (isJust, fromMaybe, catMaybes, fromJust)
import qualified Data.Vector                    as V
import           System.FilePath                ((<.>), (</>))
import           System.IO                      (hPutStrLn, stderr, hPutStr)

data CalibrateOptions = CalibrateOptions {   
      _calibrateC14Age :: Int,
      _calibrateC14Std :: Int
    }

runCalibrate :: CalibrateOptions -> IO ()
runCalibrate (CalibrateOptions c14Age c14Std) = do
    hPutStrLn stderr $ show $ uncalToPDF (UncalC14 2000 50)
    return ()

projectUncalOverCalCurve :: CalCurveSegment -> UncalPDF -> CalPDF
projectUncalOverCalCurve (CalCurveSegment calCurveMatrix) (UncalPDF pdf) =
    let years = V.map (\(x, y) -> x) pdf
        probabilities = V.map (\(x, y) -> y) pdf
        vectorMatrixMultiplication = map (M.mapCol (\row x -> x + probabilities V.! row)) [0 .. (M.ncols calCurveMatrix - 1)]
        colSum = map sum $ map (\(x) -> getCol x vectorMatrixMultiplication) [0 .. (M.ncols calCurveMatrix - 1)]
    in CalPDF years colSum
    

--calCurveMatrix :: UncalPDF -> Matrix
calCurveMatrix :: CalCurveSegment
calCurveMatrix = 
    CalCurveSegment $ M.matrix 101 101 $ \(i,j) -> 1

uncalToPDF :: UncalC14 -> UncalPDF
uncalToPDF (UncalC14 mean std) = 
    let years = V.fromList $ reverse [(mean-std) .. (mean+std)]
        probabilities = V.map (dnorm mean std) years 
    in UncalPDF $ V.zip years probabilities

dnorm :: Double -> Double -> Double -> Double 
dnorm mu sigma x = a * b
  where
    a = recip (sqrt (2 * pi * sigma2))
    b = exp ((-(realToFrac x - realToFrac mu)^2) / (2 * sigma2))
    sigma2 = realToFrac sigma^2

