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
      _calibrateUncalC14File :: [FilePath],
      _calibrateQuickOut :: Bool,
      _calibrateDensityFile :: Maybe FilePath,
      _calibrateHDRFile :: Maybe FilePath,
      _calibrateCalCurveSegmentFile :: Maybe FilePath,
      _calibrateCalCurveMatrixFile :: Maybe FilePath
    }

runCalibrate :: CalibrateOptions -> IO ()
runCalibrate (CalibrateOptions uncalDates uncalFile quickOut densityFile hdrFile calCurveSegmentFile calCurveMatrixFile) = do
    -- compile dates
    entitiesFromFile <- mapM readUncalC14FromFile uncalFile
    let dates = replaceEmptyNames $ uncalDates ++ concat entitiesFromFile
    if null dates
    then hPutStrLn stderr "Nothing to calibrate. See currycarbon calibrate -h"
    else do
        -- basic calibration
        hPutStrLn stderr "Calibrating..."
        let calCurve = loadCalCurve intcal20
            calPDFs = calibrateMany calCurve dates
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
                            show (head dates)
            let uncalPDF = uncalToPDF $ head dates
                (calCurveSegment,calCurveMatric) = prepareCalCurve calCurve uncalPDF
            when (isJust calCurveSegmentFile) $ do
                writeCalCurveFile (fromJust calCurveSegmentFile) calCurveSegment
            when (isJust calCurveMatrixFile) $ do
                writeCalCurveMatrixFile (fromJust calCurveMatrixFile) calCurveMatric
        -- finished
        hPutStrLn stderr "Done"

replaceEmptyNames :: [UncalC14] -> [UncalC14]
replaceEmptyNames xs =
    zipWith replaceName xs [1..]
    where
        replaceName :: UncalC14 -> Int -> UncalC14
        replaceName (UncalC14 name mean std) number =
            if name == "unknownSampleName"
            then UncalC14 (show number) mean std
            else UncalC14 name mean std