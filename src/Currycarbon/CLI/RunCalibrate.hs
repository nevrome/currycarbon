module Currycarbon.CLI.RunCalibrate
    (CalibrateOptions (..), runCalibrate) where

import           Currycarbon.CalCurves
import           Currycarbon.Calibration.Calibration
import           Currycarbon.Calibration.Utils
import           Currycarbon.Parsers
import           Currycarbon.SumCalibration
import           Currycarbon.Types
import           Currycarbon.Utils

import           Control.Exception                   (throwIO)
import           Control.Monad                       (unless, when)
import           Data.Maybe                          (fromJust, isJust)
import           System.IO                           (hPutStrLn, stderr)
import qualified System.Random                       as R

-- | A data type to represent the options to the CLI module function runCalibrate
data CalibrateOptions = CalibrateOptions {
        _calibrateExprs                   :: [NamedCalExpr] -- ^ String listing the uncalibrated dates that should be calibrated
      , _calibrateExprFiles               :: [FilePath] -- ^ List of files with uncalibrated dates to be calibrated
      , _calibrateCalCurveFile            :: CalCurveSelection -- ^ Either a preloaded calibration curve or a path to a .14c file
      , _calibrateCalibrationMethod       :: CalibrationMethod -- ^ Calibration algorithm that should be used
      , _calibrateAllowOutside            :: Bool -- ^ Allow calibration to run outside of the range of the calibration curve
      , _calibrateDontInterpolateCalCurve :: Bool -- ^ Don't interpolate the calibration curve
      , _calibrateDontTrimCalCurve        :: Bool -- ^ Don't trim the calibration curve before the calibration
      , _calibrateDontTrimOutCalPDF       :: Bool -- ^ Don't trim the output CalPDF
      , _calibrateQuiet                   :: Bool -- ^ Suppress the printing of calibration results to the command line
      , _calibrateStdOutEncoding          :: String -- ^ Encoding of the stdout stream (show TextEncoding)
      , _calibrateBasicFile               :: Maybe FilePath -- ^ Path to an output file (see CLI documentation)
      , _calibrateDensityFile             :: Maybe FilePath -- ^ Path to an output file
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
            calCurveSelection method allowOutside noInterpolate noTrimCalCurve noTrimOutCalPDF
            quiet encoding
            basicFile densityFile hdrFile
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
        hPutStrLn stderr $ "Curve: " ++ show calCurveSelection
        calCurve <- getCalCurve calCurveSelection
        let calConf = defaultCalConf {
              _calConfAllowOutside = allowOutside
            , _calConfInterpolateCalCurve = not noInterpolate
            , _calConfTrimCalCurveBeforeCalibration = not noTrimCalCurve
            -- previously set to: not noTrimOutCalPDF
            -- but for the command line app it's better to always do this later (see below!)
            , _calConfTrimCalPDFAfterCalibration = False
            }
        -- handle the special debug cases
        when (isJust calCurveSegmentFile || isJust calCurveMatrixFile) $ do
            case exprsRenamed of
                [NamedCalExpr _ (UnCalDate uncal)] -> do
                    let calCurveSegment = prepareCalCurveSegment (not noInterpolate) $
                            getRelevantCalCurveSegment uncal calCurve
                    when (isJust calCurveSegmentFile) $
                        writeCalCurve (fromJust calCurveSegmentFile) calCurveSegment
                    when (isJust calCurveMatrixFile) $
                        writeCalCurveMatrix (fromJust calCurveMatrixFile) $
                        makeCalCurveMatrix (uncalToPDF uncal) calCurveSegment
                _ -> do
                    throwIO $ CurrycarbonCLIException
                        "--calCurveSegFile and --calCurveMatFile only work with \
                        \a single uncalibrated radiocarbon date."
        -- run calibration
        hPutStrLn stderr "Calibrating..."
        let errorOrCalPDFs = map (evalNamedCalExpr method calConf calCurve) exprsRenamed
        -- trim output
            calRes = if not noTrimOutCalPDF
                     then map (mapEither id trimLowDensityEdgesCalPDF) errorOrCalPDFs
                     else errorOrCalPDFs
        -- prepare random number generator for age sampling
        maybeRNG <- case ageSampling of
            Nothing -> pure Nothing
            Just (maybeSeed, _, _) -> case maybeSeed of
                Nothing   -> Just <$> R.initStdGen
                Just seed -> return $ Just $ R.mkStdGen (fromIntegral seed)
        -- prepare and write the output per expression
        handleExprs ascii True calCurve maybeRNG $ zip exprsRenamed calRes
    where

        -- loop over first and subsequent expressions
        handleExprs ::
               Bool -- encoding
            -> Bool -- is this expression the first in the list of expressions?
            -> CalCurveBP
            -> Maybe R.StdGen -- rng for the age sampling seeds
            -> [(NamedCalExpr, Either CurrycarbonException CalPDF)]
            -> IO ()
        handleExprs _ _ _ _ [] = hPutStrLn stderr "Done."
        -- first expression
        handleExprs _ascii True calCurve maybeRNG (firstDate:otherDates) =
            case firstDate of
                (_, Left e) -> do
                    printE e
                    handleExprs _ascii True calCurve maybeRNG otherDates
                (namedCalExpr, Right cPDF) -> do
                    let (sampleSeed, newRNG) = drawSeed maybeRNG
                    flexOut _ascii namedCalExpr cPDF sampleSeed writeCalPDF writeCalC14CalRangeSummary writeCalC14HDR writeRandomAgeSample
                    handleExprs _ascii False calCurve newRNG otherDates
        -- subsequent expression
        handleExprs _ascii False calCurve maybeRNG (nextDate:otherDates) =
            case nextDate of
                (_, Left e) -> do
                    printE e
                    handleExprs _ascii False calCurve maybeRNG otherDates
                (namedCalExpr, Right cPDF) -> do
                    let (sampleSeed, newRNG) = drawSeed maybeRNG
                    flexOut _ascii namedCalExpr cPDF sampleSeed appendCalPDF appendCalC14CalRangeSummary appendCalC14HDR appendRandomAgeSample
                    handleExprs _ascii False calCurve newRNG otherDates

        printE :: CurrycarbonException -> IO ()
        printE e = hPutStrLn stderr $ renderCurrycarbonException e

        drawSeed :: Maybe R.StdGen -> (Maybe Int, Maybe R.StdGen)
        drawSeed maybeRNG = (\x -> (fromIntegral . fst <$> x, snd <$> x)) (R.genWord32 <$> maybeRNG)

        -- flexible expression handler
        flexOut ::
               Bool
            -> NamedCalExpr
            -> CalPDF
            -> Maybe Int
            -> (FilePath -> CalPDF -> IO ())
            -> (FilePath -> CalC14 -> IO ())
            -> (FilePath -> CalC14 -> IO ())
            -> (FilePath -> RandomAgeSample -> IO ())
            -> IO ()
        flexOut _ascii namedCalExpr calPDF maybeSeed calPDFToFile calC14CalRangeSummaryToFile calC14HDRToFile randomAgeSampleToFile = do
            case refineCalDate calPDF of
                Left e -> do
                    unless quiet $ do
                        putStrLn ("CalEXPR: " ++ renderNamedCalExpr namedCalExpr)
                        printE e
                    when (isJust basicFile) $ unless quiet $
                        hPutStrLn stderr "<!> Error: Can not create --basicFile"
                    when (isJust hdrFile) $ unless quiet $
                        hPutStrLn stderr "<!> Error: Can not create --hdrFile"
                Right calC14 -> do
                    unless quiet $ do
                        putStrLn (renderCalDatePretty _ascii (namedCalExpr, calPDF, calC14))
                    when (isJust basicFile) $
                        calC14CalRangeSummaryToFile (fromJust basicFile) calC14
                    when (isJust hdrFile) $
                        calC14HDRToFile (fromJust hdrFile) calC14
            when (isJust ageSampling && isJust maybeSeed) $ do
                let (_, nrOfSamples, path) = fromJust ageSampling
                    rng = R.mkStdGen (fromJust maybeSeed)
                    conf = AgeSamplingConf rng nrOfSamples
                case sampleAgesFromCalPDF conf calPDF of
                    Left e -> do
                        unless quiet $ do
                            printE e
                            hPutStrLn stderr "<!> Error: Can not create --samplesFile"
                    Right res -> randomAgeSampleToFile path res
            when (isJust densityFile) $
                calPDFToFile (fromJust densityFile) calPDF

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
