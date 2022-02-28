module Currycarbon.CLI.RunCalibrate
    (CalibrateOptions (..), runCalibrate) where

import           Currycarbon.CalCurves.Intcal20
import           Currycarbon.Calibration.Calibration
import           Currycarbon.Parsers
import           Currycarbon.Types
import           Currycarbon.Utils

import           Control.Monad      (when, unless)
import           Data.Either        (rights, lefts, isRight)
import           Data.Maybe         (fromJust, isJust)
import           System.IO          (hPutStrLn, stderr)

-- | A data type to represent the options to the CLI module function runCalibrate
data CalibrateOptions = CalibrateOptions {
        _calibrateUncalC14 :: [UncalC14]  -- ^ Uncalibrated dates that should be calibrated
      , _calibrateUncalC14File :: [FilePath] -- ^ List of files with uncalibrated dates to be calibrated
      , _calibrateCalCurveFile :: Maybe FilePath -- ^ Path to a .14c file
      , _calibrateCalibrationMethod :: CalibrationMethod -- ^ Calibration algorithm that should be used
      , _calibrateAllowOutside :: Bool -- ^ Allow calibration to run outside of the range of the calibration curve 
      , _calibrateDontInterpolateCalCurve :: Bool -- ^ Don't interpolate the calibration curve
      , _calibrateQuiet :: Bool -- ^ Suppress the printing of calibration results to the command line
      , _calibrateDensityFile :: Maybe FilePath -- ^ Path to an output file (see CLI documentation)
      , _calibrateHDRFile :: Maybe FilePath -- ^ Path to an output file
      , _calibrateCalCurveSegmentFile :: Maybe FilePath -- ^ Path to an output file 
      , _calibrateCalCurveMatrixFile :: Maybe FilePath -- ^ Path to an output file 
    }

-- | Interface function to trigger calibration from the command line
runCalibrate :: CalibrateOptions -> IO ()
runCalibrate (CalibrateOptions uncalDates uncalFile calCurveFile method allowOutside noInterpolate quiet densityFile hdrFile calCurveSegmentFile calCurveMatrixFile) = do
    -- compile dates
    entitiesFromFile <- mapM readUncalC14FromFile uncalFile
    let uncalDatesRenamed = replaceEmptyNames $ uncalDates ++ concat entitiesFromFile
    if null uncalDatesRenamed
    then hPutStrLn stderr "Nothing to calibrate. See currycarbon calibrate -h"
    else do
        -- basic calibration
        hPutStrLn stderr "Calibrating..."
        calCurve <- maybe (return intcal20) readCalCurveFromFile calCurveFile
        let calConf = defaultCalConf {
              _calConfMethod = method
            , _calConfAllowOutside = allowOutside
            , _calConfInterpolateCalCurve = not noInterpolate
            }
            errorOrCalPDFs = calibrateDates calConf calCurve uncalDatesRenamed
            dates = map fst $ filter (isRight . snd) $ zip uncalDatesRenamed errorOrCalPDFs
            calPDFs = rights errorOrCalPDFs
        -- cover the case of failed calibration
        if null calPDFs
        then do
            reportCalibrationErrors $ lefts errorOrCalPDFs
            hPutStrLn stderr "Calibration failed for all input dates."
        else do
            -- write density file
            when (isJust densityFile) $ do
                writeCalPDFs (fromJust densityFile) calPDFs
            -- print or write high density regions
            when (not quiet || isJust hdrFile) $ do
                let calC14s = refineCalDates calPDFs
                unless quiet $ do
                    --putStrLn $ renderCalC14s calC14s
                    putStrLn $ renderCalDatesPretty $ zip3 dates calC14s calPDFs
                when (isJust hdrFile) $ do
                    writeCalC14s (fromJust hdrFile) calC14s
            -- write calcurve segment file
            when (isJust calCurveSegmentFile || isJust calCurveMatrixFile) $ do
                hPutStrLn stderr $ "The calCurveSegment file and the calCurveMatrix file only consider the first date, " ++
                                renderUncalC14 (head dates)
                let firstC14 = head dates
                    calCurveSegment = prepareCalCurveSegment (not noInterpolate) $ getRelevantCalCurveSegment firstC14 calCurve
                when (isJust calCurveSegmentFile) $ do
                    writeCalCurve (fromJust calCurveSegmentFile) calCurveSegment
                when (isJust calCurveMatrixFile) $ do
                    writeCalCurveMatrix (fromJust calCurveMatrixFile) $ makeCalCurveMatrix (uncalToPDF firstC14) calCurveSegment
            -- finished
            reportCalibrationErrors $ lefts errorOrCalPDFs
        hPutStrLn stderr "Done"

reportCalibrationErrors :: [CurrycarbonException] -> IO ()
reportCalibrationErrors = mapM_ (hPutStrLn stderr . renderCurrycarbonException)

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