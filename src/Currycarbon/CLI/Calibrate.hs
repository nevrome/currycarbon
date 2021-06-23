module Currycarbon.CLI.Calibrate where

import           Currycarbon.Calibration
import           Currycarbon.Parsers
import           Currycarbon.Types
import           Currycarbon.Utils

import           TextPlot

data CalibrateOptions = CalibrateOptions {
      _calibrateC14Age :: Int,
      _calibrateC14Std :: Int
    }

runCalibrate :: CalibrateOptions -> IO ()
runCalibrate (CalibrateOptions c14Age c14Std) = do
    calCurve <- readCalCurve
    let calPDF = calibrate calCurve (UncalC14 c14Age c14Std)
    -- plots
    --plotCalCurveSegment calCurveSegment
    --plotCalPDF calPDF
    writeCalPDF calPDF
    return ()

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