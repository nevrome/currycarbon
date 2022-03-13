{-# LANGUAGE BangPatterns #-}

module Currycarbon.CLI.RunCalibrate
    (CalibrateOptions (..), runCalibrate) where

import           Currycarbon.CalCurves.Intcal20
import           Currycarbon.Calibration.Calibration
import           Currycarbon.Parsers
import           Currycarbon.Types
import           Currycarbon.Utils

import           Control.Monad      (when, unless)
import           Data.Either        (rights, lefts, isRight)
import           Data.Foldable      (forM_)
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
    then hPutStrLn stderr "Nothing to calibrate. See currycarbon -h for help"
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
        -- print or write output
        handleDates True calCurve $ zip uncalDatesRenamed errorOrCalPDFs
        where
            handleDates :: Bool -> CalCurveBP -> [(UncalC14, Either CurrycarbonException CalPDF)] -> IO ()
            handleDates _ _ [] = hPutStrLn stderr "Done."
            handleDates True calCurve (x:xs) = do
                case x of
                    (_, Left ex) -> do
                        hPutStrLn stderr $ renderCurrycarbonException ex
                        handleDates True calCurve xs
                    (uncal, Right cPDF) -> do
                        handleFirstDate calCurve uncal cPDF
                        handleDates False calCurve xs
            handleDates False calCurve (x:xs) = do
                case x of
                    (_, Left ex) -> do
                        hPutStrLn stderr $ renderCurrycarbonException ex
                        handleDates False calCurve xs
                    (uncal, Right cPDF) -> do
                        handleOtherDate uncal cPDF
                        handleDates False calCurve xs
            handleFirstDate :: CalCurveBP -> UncalC14 -> CalPDF -> IO ()
            handleFirstDate calCurve uncal calPDF = do
                -- write calcurve segment or calcurve matrix file
                when (isJust calCurveSegmentFile || isJust calCurveMatrixFile) $ do
                    hPutStrLn stderr $ 
                        "The calCurveSegment file and the calCurveMatrix file only consider the first date, " ++
                        renderUncalC14 uncal
                    let calCurveSegment = prepareCalCurveSegment (not noInterpolate) $ getRelevantCalCurveSegment uncal calCurve
                    when (isJust calCurveSegmentFile) $ do
                        writeCalCurve (fromJust calCurveSegmentFile) calCurveSegment
                    when (isJust calCurveMatrixFile) $ do
                        writeCalCurveMatrix (fromJust calCurveMatrixFile) $ makeCalCurveMatrix (uncalToPDF uncal) calCurveSegment
                -- prettyprint high density regions
                let calC14 = refineCalDate calPDF
                unless quiet $ putStrLn $ renderCalDatePretty (uncal, calPDF, calC14)
                -- create hdr file
                when (isJust hdrFile) $ writeCalC14 (fromJust hdrFile) calC14
                -- create density file
                when (isJust densityFile) $ writeCalPDF (fromJust densityFile) calPDF
            handleOtherDate :: UncalC14 -> CalPDF -> IO ()
            handleOtherDate uncal calPDF = do
                -- prettyprint high density regions
                let calC14 = refineCalDate calPDF
                unless quiet $ putStrLn $ renderCalDatePretty (uncal, calPDF, calC14)
                -- append to hdr file
                when (isJust hdrFile) $ appendCalC14 (fromJust hdrFile) calC14
                -- append to density file
                when (isJust densityFile) $ appendCalPDF (fromJust densityFile) calPDF                  
                   
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