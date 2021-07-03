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
    -- basic calibration
    hPutStrLn stderr "Loading calibration curve"
    let calCurve = loadCalCurve intcal20
    hPutStrLn stderr "Calibrating"
    let calPDFs = calibrateMany calCurve uncalDate
    -- write density file
    when (isJust densityFile) $ do
        hPutStrLn stderr "Writing density file"
        writeCalPDFs (fromJust densityFile) calPDFs
    -- print or write high density regions
    when (quickOut || isJust hdrFile) $ do
        hPutStrLn stderr "Calculating HDR regions"
        let calC14 = refineCal calPDFs
        when quickOut $ do
            hPutStrLn stdout $ renderCalC14s calC14
        when (isJust hdrFile) $ do
            hPutStrLn stderr "Writing hdr file"
            writeCalC14 (fromJust hdrFile) calC14
    -- write calcurve segment file
    when (isJust calCurveSegmentFile || isJust calCurveMatrixFile) $ do
        hPutStrLn stderr $ "The calCurveSegment file and the calCurveMatrix file only consider the first date: " ++
                           show uncalDate
        let uncalPDF = uncalToPDF $ head uncalDate
            (calCurveSegment,calCurveMatric) = prepareCalCurve calCurve uncalPDF
        writeCalCurveFile (fromJust calCurveSegmentFile) calCurveSegment
        writeCalCurveMatrixFile (fromJust calCurveMatrixFile) calCurveMatric
    hPutStrLn stderr "Done"

