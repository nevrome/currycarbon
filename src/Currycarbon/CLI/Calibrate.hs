module Currycarbon.CLI.Calibrate where

import           Currycarbon.CalCurves.Intcal20
import           Currycarbon.Calibration
import           Currycarbon.Parsers
import           Currycarbon.Types
import           Currycarbon.Utils

import           Control.Monad      (when)
import           System.FilePath    ((</>))
import           System.Directory   (createDirectoryIfMissing)
import           TextPlot
import Data.Maybe (fromJust)
import Data.List (sort)

data CalibrateOptions = CalibrateOptions {
      _calibrateUncalC14 :: [UncalC14],
      _calibrateOutFile :: FilePath,
      _calibrateExplore :: Bool,
      _calibrateExploreDir :: FilePath
    }

runCalibrate :: CalibrateOptions -> IO ()
runCalibrate (CalibrateOptions uncalC14s outFile explore exploreDir) = do
    -- normal mode
    let calCurve = loadCalCurve intcal20
        calPDFs = calibrateMany calCurve uncalC14s
    writeCalPDFs outFile calPDFs
    -- single date exploration
    when explore $ do
        let (calPDF,calCurveSegment,calCurveMatrix) = calibrate calCurve $ head uncalC14s
        createDirectoryIfMissing True exploreDir
        plotCalCurveSegment calCurveSegment
        plotCalPDF calPDF
        writeCalCurve (exploreDir </> "calCurveInterpolated.csv") calCurveSegment
        writeCalCurveMatrixFile (exploreDir </> "calCurveMatrix.csv") calCurveMatrix

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
        getCalPDFValue (CalPDF _ obs) x = 
            snd $ head $ filter (\(y,_) -> round x == y) obs