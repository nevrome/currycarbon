module Currycarbon.CLI.CLICalibrate where

import           Currycarbon.CalCurves.Intcal20
import           Currycarbon.Calibration
import           Currycarbon.Parsers
import           Currycarbon.Types

import           Control.Monad      (when)
import           Data.Maybe         (fromJust, isJust)
import           System.IO          (hPutStrLn, stderr, stdout)

data CalibrateOptions = CalibrateOptions {
      _calibrateUncalC14 :: [UncalC14],
      _calibrateQuickOut :: Bool,
      _calibrateDensityFile :: Maybe FilePath,
      _calibrateHDRFile :: Maybe FilePath,
      _calibrateCalCurveSegmentFile :: Maybe FilePath,
      _calibrateCalCurveMatrixFile :: Maybe FilePath
    }

runCalibrate :: CalibrateOptions -> IO ()
runCalibrate (CalibrateOptions uncalDate quickOut densityFile hdrFile calCurveSegmentFile calCurveMatrixFile) = do
    hPutStrLn stderr "Calibrating..."
    -- basic calibration
    let calCurve = loadCalCurve intcal20
        calPDFs = calibrateMany calCurve uncalDate
    -- write density file
    when (isJust densityFile) $ do
        writeCalPDFs (fromJust densityFile) calPDFs
    -- print or write high density regions
    when (quickOut || isJust hdrFile) $ do
        let calC14 = refineCal calPDFs
        when quickOut $ do
            hPutStrLn stdout $ renderCalC14s calC14
        when (isJust hdrFile) $ do
            writeCalC14 (fromJust hdrFile) calC14
    -- write calcurve segment file
    when (isJust calCurveSegmentFile || isJust calCurveMatrixFile) $ do
        hPutStrLn stderr $ "The calCurveSegment file and the calCurveMatrix file only consider the first date: " ++
                           show (head uncalDate)
        let uncalPDF = uncalToPDF $ head uncalDate
            (calCurveSegment,calCurveMatric) = prepareCalCurve calCurve uncalPDF
        when (isJust calCurveSegmentFile) $ do
            writeCalCurveFile (fromJust calCurveSegmentFile) calCurveSegment
        when (isJust calCurveMatrixFile) $ do
            writeCalCurveMatrixFile (fromJust calCurveMatrixFile) calCurveMatric
    -- finished
    hPutStrLn stderr "Done"

