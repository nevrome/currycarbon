module Currycarbon.CLI.RunCalibrate
    (CalibrateOptions (..), runCalibrate) where

import           Currycarbon.CalCurves.Intcal20
import           Currycarbon.Calibration
import           Currycarbon.Parsers
import           Currycarbon.Types

import           Control.Monad      (when, unless)
import           Data.Maybe         (fromJust, isJust)
import           System.IO          (hPutStrLn, stderr)

-- | A data type to represent the options to the CLI module function runCalibrate
data CalibrateOptions = CalibrateOptions {
        _calibrateUncalC14 :: [UncalC14]  -- ^ Uncalibrated dates that should be calibrated
      , _calibrateUncalC14File :: [FilePath] -- ^ List of files with uncalibrated dates to be calibrated
      , _calibrateCalCurveFile :: Maybe FilePath -- ^ Path to a .14c file
      , _calibrateInterpolateCalCurve :: Bool -- ^ Interpolate the calibration curve to get a year-wise result
      , _calibrateQuiet :: Bool -- ^ Suppress the printing of calibration results to the command line
      , _calibrateDensityFile :: Maybe FilePath -- ^ Path to an output file (see CLI documentation)
      , _calibrateHDRFile :: Maybe FilePath -- ^ Path to an output file
      , _calibrateCalCurveSegmentFile :: Maybe FilePath -- ^ Path to an output file 
      , _calibrateCalCurveMatrixFile :: Maybe FilePath -- ^ Path to an output file 
    }

-- | Interface function to trigger calibration from the command line
runCalibrate :: CalibrateOptions -> IO ()
runCalibrate (CalibrateOptions uncalDates uncalFile calCurveFile interpolate quiet densityFile hdrFile calCurveSegmentFile calCurveMatrixFile) = do
    -- compile dates
    entitiesFromFile <- mapM readUncalC14FromFile uncalFile
    let dates = replaceEmptyNames $ uncalDates ++ concat entitiesFromFile
    if null dates
    then hPutStrLn stderr "Nothing to calibrate. See currycarbon calibrate -h"
    else do
        -- basic calibration
        hPutStrLn stderr "Calibrating..."
        calCurve <- maybe (return $ loadCalCurve intcal20) readCalCurve calCurveFile
        let calPDFs = calibrateDates interpolate calCurve dates
        -- write density file
        when (isJust densityFile) $ do
            writeCalPDFs (fromJust densityFile) calPDFs
        -- print or write high density regions
        when (not quiet || isJust hdrFile) $ do
            let calC14s = refineCalDates calPDFs
            unless quiet $ do
                putStrLn $ renderCalC14s calC14s
            when (isJust hdrFile) $ do
                writeCalC14s (fromJust hdrFile) calC14s
        -- write calcurve segment file
        when (isJust calCurveSegmentFile || isJust calCurveMatrixFile) $ do
            hPutStrLn stderr $ "The calCurveSegment file and the calCurveMatrix file only consider the first date: " ++
                            show (head dates)
            let uncalPDF = uncalToPDF $ head dates
                (calCurveSegment,calCurveMatric) = prepareCalCurve interpolate calCurve uncalPDF
            when (isJust calCurveSegmentFile) $ do
                writeCalCurveFile (fromJust calCurveSegmentFile) calCurveSegment
            when (isJust calCurveMatrixFile) $ do
                writeCalCurveMatrixFile (fromJust calCurveMatrixFile) calCurveMatric
        -- finished
        hPutStrLn stderr "Done"

-- | Helper function to replace empty input names with a sequence of numbers, 
-- to get each input date an unique identifier
replaceEmptyNames :: [UncalC14] -> [UncalC14]
replaceEmptyNames xs =
    zipWith replaceName xs [1..]
    where
        replaceName :: UncalC14 -> Int -> UncalC14
        replaceName (UncalC14 name mean std) number =
            if name == "unknownSampleName"
            then UncalC14 (show number) mean std
            else UncalC14 name mean std