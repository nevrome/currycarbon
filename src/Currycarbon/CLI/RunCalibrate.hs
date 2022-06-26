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
    let exprsRenamed = replaceEmptyNames $ exprs ++ concat exprsFromFile
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
        handleDates True calCurve $ zip exprsRenamed errorOrCalPDFs
        where
            -- the bool manages if a date is the first, calibratable date
            handleDates :: Bool -> CalCurveBP -> [(CalExpr, Either CurrycarbonException CalPDF)] -> IO ()
            handleDates _ _ [] = hPutStrLn stderr "Done."
            handleDates True calCurve (firstDate:otherDates) = case firstDate of
                (_, Left e)           -> printE e                       >> handleDates True  calCurve otherDates
                (calExpr, Right cPDF) -> firstOut calCurve calExpr cPDF >> handleDates False calCurve otherDates
            handleDates False calCurve (firstDate:otherDates) = case firstDate of
                (_, Left e)           -> printE e                       >> handleDates False calCurve otherDates
                (calExpr, Right cPDF) -> otherOut calExpr cPDF          >> handleDates False calCurve otherDates
            firstOut :: CalCurveBP -> CalExpr -> CalPDF -> IO ()
            firstOut calCurve calExpr@(UnCalDate uncal) calPDF = do
                flexOut calExpr calPDF writeCalPDF writeCalC14
                when (isJust calCurveSegmentFile || isJust calCurveMatrixFile) $ do
                    hPutStrLn stderr $
                        "Warning: The calCurveSegment file and the calCurveMatrix file only consider the first date, " ++
                        renderUncalC14 uncal
                    let calCurveSegment = prepareCalCurveSegment (not noInterpolate) $ getRelevantCalCurveSegment uncal calCurve
                    when (isJust calCurveSegmentFile) $
                        writeCalCurve (fromJust calCurveSegmentFile) calCurveSegment
                    when (isJust calCurveMatrixFile) $
                        writeCalCurveMatrix (fromJust calCurveMatrixFile) $
                        makeCalCurveMatrix (uncalToPDF uncal) calCurveSegment
            firstOut _ calExpr calPDF = do
                flexOut calExpr calPDF writeCalPDF writeCalC14
                when (isJust calCurveSegmentFile || isJust calCurveMatrixFile) $ do
                    hPutStrLn stderr $ "Warning: The calCurveSegment file and the calCurveMatrix file can only be produced for simple dates"
            otherOut :: CalExpr -> CalPDF -> IO ()
            otherOut calExpr calPDF =
                flexOut calExpr calPDF appendCalPDF appendCalC14
            flexOut :: CalExpr -> CalPDF -> (FilePath -> CalPDF -> IO ()) -> (FilePath -> CalC14 -> IO ()) -> IO ()
            flexOut calExpr calPDF calPDFToFile calC14ToFile = do
                case refineCalDate calPDF of
                    Nothing -> do
                        unless quiet $ do
                            putStrLn $ renderCalExpr calExpr
                            hPutStrLn stderr "Warning: Could not calculate meaningful HDRs for this expression. Check --densityFile."
                        when (isJust hdrFile)     $ unless quiet $ hPutStrLn stderr "Nothing written to the HDR file"
                        when (isJust densityFile) $ calPDFToFile (fromJust densityFile) calPDF
                    Just calC14 -> do
                        unless quiet              $ putStrLn $ renderCalDatePretty (calExpr, calPDF, calC14)
                        when (isJust hdrFile)     $ calC14ToFile (fromJust hdrFile) calC14
                        when (isJust densityFile) $ calPDFToFile (fromJust densityFile) calPDF
            printE :: CurrycarbonException -> IO ()
            printE e = hPutStrLn stderr $ renderCurrycarbonException e

-- | Helper function to replace empty input names with a sequence of numbers, 
-- to get each input date an unique identifier
replaceEmptyNames :: [CalExpr] -> [CalExpr]
replaceEmptyNames = zipWith (replaceName . show) ([1..] :: [Integer])
    where
        replaceName :: String -> CalExpr -> CalExpr
        replaceName i (UnCalDate (UncalC14 name x y)) =
            if name == "unknownSampleName"
            then UnCalDate $ UncalC14 i x y
            else UnCalDate $ UncalC14 name x y
        replaceName i (CalDate (CalPDF name x y)) = 
            if name == "unknownSampleName"
            then CalDate $ CalPDF i x y
            else CalDate $ CalPDF name x y
        replaceName i (SumCal a b)     = SumCal (replaceName (i ++ "s") a) (replaceName (i ++ "S") b)
        replaceName i (ProductCal a b) = ProductCal (replaceName (i ++ "p") a) (replaceName (i ++ "P") b)