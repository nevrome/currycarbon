{-# LANGUAGE BangPatterns #-}

module Currycarbon.CLI.RunCalibrate
    (CalibrateOptions (..), runCalibrate) where

import           Currycarbon.CalCurves.Intcal20
import           Currycarbon.Calibration.Calibration
import           Currycarbon.Parsers
import           Currycarbon.SumCalibration
import           Currycarbon.Types
import           Currycarbon.Utils

import           Control.Monad                       (unless, when)
import           Data.Maybe                          (fromJust, fromMaybe,
                                                      isJust)
import           System.IO                           (hPutStrLn, stderr, stdout)
import qualified System.Random                       as R

-- | A data type to represent the options to the CLI module function runCalibrate
data CalibrateOptions = CalibrateOptions {
        _calibrateExprs                   :: [NamedCalExpr] -- ^ String listing the uncalibrated dates that should be calibrated
      , _calibrateExprFiles               :: [FilePath] -- ^ List of files with uncalibrated dates to be calibrated
      , _calibrateCalCurveFile            :: Maybe FilePath -- ^ Path to a .14c file
      , _calibrateCalibrationMethod       :: CalibrationMethod -- ^ Calibration algorithm that should be used
      , _calibrateAllowOutside            :: Bool -- ^ Allow calibration to run outside of the range of the calibration curve
      , _calibrateDontInterpolateCalCurve :: Bool -- ^ Don't interpolate the calibration curve
      , _calibrateQuiet                   :: Bool -- ^ Suppress the printing of calibration results to the command line
      , _calibrateStdOutEncoding          :: String -- ^ Encoding of the stdout stream (show TextEncoding)
      , _calibrateDensityFile             :: Maybe FilePath -- ^ Path to an output file (see CLI documentation)
      , _calibrateHDRFile                 :: Maybe FilePath -- ^ Path to an output file
      , _calibrateAgeSampling             :: Maybe (Maybe Word, Word, FilePath) -- ^ Settings for the age sampling
      , _calibrateCalCurveSegmentFile     :: Maybe FilePath -- ^ Path to an output file
      , _calibrateCalCurveMatrixFile      :: Maybe FilePath -- ^ Path to an output file
    }

-- | Interface function to trigger calibration from the command line
runCalibrate :: CalibrateOptions -> IO ()
runCalibrate (
        CalibrateOptions
            exprs exprFiles
            calCurveFile method allowOutside noInterpolate
            quiet encoding
            densityFile hdrFile
            ageSampling
            calCurveSegmentFile calCurveMatrixFile
        ) = do
    let ascii = encoding /= "UTF-8"
    -- compile dates
    exprsFromFile <- mapM readNamedCalExprsFromFile exprFiles
    let exprsCombined = exprs ++ concat exprsFromFile
        exprsRenamed = replaceEmptyNames exprsCombined
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
        let errorOrCalPDFs = map (evalNamedCalExpr calConf calCurve) exprsRenamed
        -- prepare the output depending on many settings
        handleDates ascii True calCurve $ zip exprsRenamed errorOrCalPDFs
    where

        -- split first and subsequent expressions
        handleDates ::
               Bool -- encoding
            -> Bool -- is this expression the first in the list of expressions?
            -> CalCurveBP
            -> [(NamedCalExpr, Either CurrycarbonException CalPDF)]
            -> IO ()
        handleDates _ _ _ [] = hPutStrLn stderr "Done."
        -- first expression
        handleDates _ascii True calCurve (firstDate:otherDates) =
            case firstDate of
                (_,       Left e)     -> printE e
                                         >> handleDates _ascii True  calCurve otherDates
                (namedCalExpr, Right cPDF) -> firstOut _ascii calCurve namedCalExpr cPDF
                                         >> handleDates _ascii False calCurve otherDates
        -- subsequent expression
        handleDates _ascii False calCurve (firstDate:otherDates) =
            case firstDate of
                (_,       Left e)     -> printE e
                                         >> handleDates _ascii False calCurve otherDates
                (namedCalExpr, Right cPDF) -> otherOut _ascii namedCalExpr cPDF
                                         >> handleDates _ascii False calCurve otherDates

        printE :: CurrycarbonException -> IO ()
        printE e = hPutStrLn stderr $ renderCurrycarbonException e

        -- handle the first expression
        firstOut :: Bool -> CalCurveBP -> NamedCalExpr -> CalPDF -> IO ()
        -- is the first expression a simple, single date?
        firstOut _ascii calCurve namedCalExpr@(NamedCalExpr _ (UnCalDate uncal)) calPDF = do
            flexOut _ascii namedCalExpr calPDF writeCalPDF writeCalC14 writeRandomAgeSample
            -- this is what makes the first sample special:
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
        firstOut _ascii _ namedCalExpr calPDF = do
            flexOut _ascii namedCalExpr calPDF writeCalPDF writeCalC14 writeRandomAgeSample
            when (isJust calCurveSegmentFile || isJust calCurveMatrixFile) $ do
                hPutStrLn stderr "Warning: The calCurveSegment file and the calCurveMatrix file can only be produced for simple dates"

        -- handle subsequent expressions
        otherOut :: Bool -> NamedCalExpr -> CalPDF -> IO ()
        otherOut _ascii namedCalExpr calPDF =
            flexOut _ascii namedCalExpr calPDF appendCalPDF appendCalC14 appendRandomAgeSample

        -- flexible expression handler
        flexOut ::
               Bool
            -> NamedCalExpr -> CalPDF
            -> (FilePath -> CalPDF -> IO ())
            -> (FilePath -> CalC14 -> IO ())
            -> (FilePath -> RandomAgeSample -> IO ())
            -> IO ()
        flexOut _ascii namedCalExpr calPDF calPDFToFile calC14ToFile randomAgeSampleToFile = do
            -- try to refine calPDF
            case refineCalDate calPDF of
                -- refining did not work
                Nothing -> do
                    unless quiet $ do
                        hPutStrLn stdout (renderNamedCalExpr namedCalExpr)
                        hPutStrLn stderr "Warning: Could not calculate meaningful HDRs for this expression. \
                                          \Check --densityFile."
                    when (isJust hdrFile) $
                        unless quiet $ hPutStrLn stderr "Nothing written to the HDR file"
                    when (isJust densityFile) $
                        calPDFToFile (fromJust densityFile) calPDF
                    when (isJust ageSampling) $
                        runAgeSampling (fromJust ageSampling) calPDF randomAgeSampleToFile
                -- refining did work
                Just calC14 -> do
                    unless quiet $ do
                        hPutStrLn stdout (renderCalDatePretty _ascii (namedCalExpr, calPDF, calC14))
                    when (isJust hdrFile) $
                        calC14ToFile (fromJust hdrFile) calC14
                    when (isJust densityFile) $
                        calPDFToFile (fromJust densityFile) calPDF
                    when (isJust ageSampling) $
                        runAgeSampling (fromJust ageSampling) calPDF randomAgeSampleToFile

        runAgeSampling ::
               (Maybe Word, Word, FilePath)
            -> CalPDF
            -> (FilePath -> RandomAgeSample -> IO ())
            -> IO ()
        runAgeSampling (maybeSeed, nrOfSamples, path) calPDF randomAgeSampleToFile = do
            rng <- case maybeSeed of
                Just s  -> pure $ R.mkStdGen (fromIntegral s)
                Nothing -> R.initStdGen
            let conf = AgeSamplingConf rng nrOfSamples
                samplingResult = sampleAgesFromCalPDF conf calPDF
            randomAgeSampleToFile path samplingResult


-- | Helper function to replace empty input names with a sequence of numbers,
-- to get each input date an unique identifier
replaceEmptyNames :: [NamedCalExpr] -> [NamedCalExpr]
replaceEmptyNames = zipWith (modifyNamedExpr . show) ([1..] :: [Integer])
    where
        modifyNamedExpr :: String -> NamedCalExpr -> NamedCalExpr
        modifyNamedExpr i nexpr =
            if _exprID nexpr == ""
            then nexpr { _exprID = i, _expr = replaceName i (_expr nexpr) }
            else nexpr {              _expr = replaceName i (_expr nexpr) }
        replaceName :: String -> CalExpr -> CalExpr
        replaceName i (UnCalDate (UncalC14 name x y)) =
            if name == ""
            then UnCalDate $ UncalC14 i x y
            else UnCalDate $ UncalC14 name x y
        replaceName i (WindowBP (TimeWindowBP name start stop)) =
            if name == ""
            then WindowBP $ TimeWindowBP i start stop
            else WindowBP $ TimeWindowBP name start stop
        replaceName i (WindowBCAD (TimeWindowBCAD name start stop)) =
            if name == ""
            then WindowBCAD $ TimeWindowBCAD i start stop
            else WindowBCAD $ TimeWindowBCAD name start stop
        replaceName i (CalDate (CalPDF name x y)) =
            if name == ""
            then CalDate $ CalPDF i x y
            else CalDate $ CalPDF name x y
        replaceName i (SumCal a b)     = SumCal (replaceName (i ++ "s") a) (replaceName (i ++ "S") b)
        replaceName i (ProductCal a b) = ProductCal (replaceName (i ++ "p") a) (replaceName (i ++ "P") b)
