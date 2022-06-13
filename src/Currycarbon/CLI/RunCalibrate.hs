{-# LANGUAGE BangPatterns #-}

module Currycarbon.CLI.RunCalibrate
    (CalibrateOptions (..), runCalibrate) where

import           Currycarbon.CalCurves.Intcal20
import           Currycarbon.Calibration.Calibration
import           Currycarbon.Parsers
import           Currycarbon.SumCalibration
import           Currycarbon.Types
import           Currycarbon.Utils

import           Control.Monad      (when, unless)
import           Data.Either        (rights, lefts, isRight)
import           Data.Maybe         (catMaybes)
import           Data.Foldable      (forM_)
import           Data.Maybe         (fromJust, isJust, fromMaybe)
import           System.IO          (hPutStrLn, stderr)

-- | A data type to represent the options to the CLI module function runCalibrate
data CalibrateOptions = CalibrateOptions {
        _calibrateExprs :: [CalExpr] -- ^ String listing the uncalibrated dates that should be calibrated
      , _calibrateExprFiles :: [FilePath] -- ^ List of files with uncalibrated dates to be calibrated
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
runCalibrate (CalibrateOptions exprs exprFiles calCurveFile method allowOutside noInterpolate quiet densityFile hdrFile calCurveSegmentFile calCurveMatrixFile) = do
    -- compile dates
    exprsFromFile <- mapM readCalExprFromFile exprFiles
    --let uncalDatesRenamed = replaceEmptyNames $ uncalDates ++ concat entitiesFromFile
    let exprsRenamed = exprs ++ concat exprsFromFile
    if null exprsRenamed
    then hPutStrLn stderr "Nothing to calibrate. See currycarbon -h for help"
    else do
        -- prep data
        hPutStrLn stderr $ "Method: " ++ show method
        hPutStrLn stderr $ "Curve: " ++ fromMaybe "IntCal20" calCurveFile
        calCurve <- maybe (return intcal20) readCalCurveFromFile calCurveFile
        let calConf = defaultCalConf {
              _calConfMethod = method
            , _calConfAllowOutside = allowOutside
            , _calConfInterpolateCalCurve = not noInterpolate
            }
        -- run calibration
        hPutStrLn stderr "Calibrating..."
        let errorOrCalPDFs = map (evalCalExpr calConf calCurve) exprsRenamed
          --let calPDF = head $ catMaybes $ map (evalCalExpr calConf calCurve) exprsRenamed
          --    calC14 = refineCalDate calPDF
          --putStrLn $ renderCalDatePretty (UncalC14 "hu" 0 0, calPDF, calC14)
        handleDates True calCurve $ zip exprsRenamed errorOrCalPDFs
        where
            handleDates :: Bool -> CalCurveBP -> [(CalExpr, Either CurrycarbonException CalPDF)] -> IO ()
            handleDates _ _ [] = hPutStrLn stderr "Done."
            handleDates True calCurve (x:xs) = case x of
                (_, Left ex)       -> printEx ex                         >> handleDates True  calCurve xs
                (expr, Right cPDF) -> handleFirstDate calCurve expr cPDF >> handleDates False calCurve xs
            handleDates False calCurve (x:xs) = case x of
                (_, Left ex)       -> printEx ex                         >> handleDates False calCurve xs
                (expr, Right cPDF) -> handleOtherDate expr cPDF          >> handleDates False calCurve xs
            handleFirstDate :: CalCurveBP -> CalExpr -> CalPDF -> IO ()
            handleFirstDate calCurve expr@(UnCalDate uncal) calPDF = do
                -- calcurve segment or calcurve matrix file
                if isJust calCurveSegmentFile || isJust calCurveMatrixFile 
                then do
                    hPutStrLn stderr $ 
                        "The calCurveSegment file and the calCurveMatrix file only consider the first date, " ++
                        renderUncalC14 uncal
                    let calCurveSegment = prepareCalCurveSegment (not noInterpolate) $ getRelevantCalCurveSegment uncal calCurve
                    when (isJust calCurveSegmentFile) $ 
                        writeCalCurve (fromJust calCurveSegmentFile) calCurveSegment
                    when (isJust calCurveMatrixFile) $ 
                        writeCalCurveMatrix (fromJust calCurveMatrixFile) $ 
                        makeCalCurveMatrix (uncalToPDF uncal) calCurveSegment
                else do
                    -- other output
                    let calC14 = refineCalDate calPDF
                    unless quiet              $ putStrLn $ renderCalDatePretty (expr, calPDF, calC14)
                    when (isJust hdrFile)     $ writeCalC14 (fromJust hdrFile) calC14
                    when (isJust densityFile) $ writeCalPDF (fromJust densityFile) calPDF
            handleFirstDate _ _ _ = hPutStrLn stderr $ 
                        "The calCurveSegment file and the calCurveMatrix file can only be created for \
                        \a DATEEXPR with only one uncalibrated date."
            handleOtherDate :: CalExpr -> CalPDF -> IO ()
            handleOtherDate expr calPDF = do
                let calC14 = refineCalDate calPDF
                unless quiet              $ putStrLn $ renderCalDatePretty (expr, calPDF, calC14)
                when (isJust hdrFile)     $ appendCalC14 (fromJust hdrFile) calC14
                when (isJust densityFile) $ appendCalPDF (fromJust densityFile) calPDF
            printEx :: CurrycarbonException -> IO ()
            printEx ex = hPutStrLn stderr $ renderCurrycarbonException ex

-- | Helper function to replace empty input names with a sequence of numbers, 
-- to get each input date an unique identifier
-- replaceEmptyNames :: [UncalC14] -> [UncalC14]
-- replaceEmptyNames xs =
--     zipWith replaceName xs [1..]
--     where
--         replaceName :: UncalC14 -> Int -> UncalC14
--         replaceName (UncalC14 name mean std) number =
--             if name == "unknownSampleName"
--             then UncalC14 (show number) mean std
--             else UncalC14 name mean std