{-# LANGUAGE OverloadedStrings #-}

import           Currycarbon.CLI.RunCalibrate       (runCalibrate, 
                                                     CalibrateOptions (..))
import           Currycarbon.Parsers
import           Currycarbon.Types
import           Currycarbon.Utils
import           Paths_currycarbon                  (version)

import           Control.Exception                  (catch)
import           Data.Version                       (showVersion)
import qualified Options.Applicative                as OP
import           System.Exit                        (exitFailure)
import           System.IO                          (hPutStrLn, stderr, stdout, hGetEncoding)
-- * CLI interface configuration
--
-- $cliInterface
--
-- This module contains the necessary code to configure the currycarbon CLI interface

-- data types
data Options = CmdCalibrate CalibrateOptions

-- CLI interface configuration
main :: IO ()
main = do
    -- check stdout encoding for the CLI plot
    stdOutEncoding <- hGetEncoding stdout
    let encoding = maybe "unknown" show stdOutEncoding
    hPutStrLn stderr $ "currycarbon v" ++ showVersion version ++ " (" ++ encoding ++ ")"
    -- prepare input parsing
    cmdOpts <- OP.customExecParser p optParserInfo
    catch (runCmd encoding cmdOpts) handler
    where
        p = OP.prefs OP.showHelpOnEmpty
        handler :: CurrycarbonException -> IO ()
        handler e = do
            hPutStrLn stderr $ renderCurrycarbonException e
            exitFailure

runCmd :: String -> Options -> IO ()
runCmd enc o = case o of
    CmdCalibrate opts -> runCalibrate opts {_calibrateStdOutEncoding = enc}

optParserInfo :: OP.ParserInfo Options
optParserInfo = OP.info (OP.helper <*> versionOption <*> optParser) (
    OP.briefDesc <>
    OP.progDesc "Intercept calibration of radiocarbon dates"
    )

versionOption :: OP.Parser (a -> a)
versionOption = OP.infoOption (showVersion version) (OP.long "version" <> OP.help "Show version")

optParser :: OP.Parser Options
optParser = CmdCalibrate <$> calibrateOptParser

calibrateOptParser :: OP.Parser CalibrateOptions
calibrateOptParser = CalibrateOptions <$> optParseCalExprString
                                      <*> optParseCalExprFromFile
                                      <*> optParseCalCurveFromFile
                                      <*> optParseCalibrationMethod
                                      <*> optParseAllowOutside
                                      <*> optParseDontInterpolateCalCurve
                                      <*> optParseQuiet
                                      <*> pure "unknown"
                                      <*> optParseDensityFile
                                      <*> optParseHDRFile
                                      <*> optParseCalCurveSegmentFile
                                      <*> optParseCalCurveMatrixFile

-- ** Input parsing functions
--
-- $inputParsing
--
-- These functions define and handle the CLI input arguments

optParseCalExprString :: OP.Parser [CalExpr]
optParseCalExprString = concat <$> OP.many (OP.argument (OP.eitherReader readCalExpr) (
    OP.metavar "DATE" <>
    OP.help "A string with one or multiple uncalibrated dates of \
            \the form \"<sample name>,<mean age BP>,<one sigma standard deviation>\" \
            \where <sample name> is optional (e.g. \"S1,4000,50\"). \
            \Multiple dates can be listed separated by \";\" (e.g. \"S1,4000,50; 3000,25; S3,1000,20\"). \
            \To sum or multiply the post calibration probability distributions, dates can be combined with \
            \\"+\" or \"*\" (e.g. \"4000,50 + 4100,100\"). \
            \These expressions can be combined arbitrarily. Parentheses can be added to specify the order \
            \of operations (e.g. \"(4000,50 + 4100,100) * 3800,50\")"
    ))

optParseCalExprFromFile :: OP.Parser [FilePath]
optParseCalExprFromFile = OP.many (OP.strOption (
    OP.long "inputFile" <>
    OP.short 'i' <>
    OP.help "A file with a list of calibration expressions. \
            \Formated just as DATE, but with a new line for each input date. \
            \DATE and --inputFile can be combined and you can provide multiple instances of --inputFile"
    ))

optParseCalCurveFromFile :: OP.Parser (Maybe FilePath)
optParseCalCurveFromFile = OP.option (Just <$> OP.str) (
    OP.long "calibrationCurveFile" <>
    OP.help "Path to an calibration curve file in .14c format. \
            \The calibration curve will be read and used for calibration. \
            \If no file is provided, currycarbon will use the intcal20 curve." <>
    OP.value Nothing
    )

optParseCalibrationMethod :: OP.Parser CalibrationMethod
optParseCalibrationMethod = OP.option (OP.eitherReader readCalibrationMethod) (
    OP.long "method" <>
    OP.help "The calibration algorithm that should be used: \
            \\"<Method>,<Distribution>,<NumberOfDegreesOfFreedom>\". \
            \The default setting is equivalent to \"Bchron,StudentT,100\" \
            \which copies the algorithm implemented in the Bchron R package. \
            \Alternatively we implemented  \"MatrixMult\", which comes without further arguments. \
            \For the Bchron algorithm with a normal distribution (\"Bchron,Normal\") \
            \the degrees of freedom argument is not relevant" <>
    OP.value (Bchron $ StudentTDist 100)
    )

optParseAllowOutside :: OP.Parser (Bool)
optParseAllowOutside = OP.switch (
    OP.long "allowOutside" <> 
    OP.help "Allow calibrations to run outside the range of the calibration curve"
    )

optParseDontInterpolateCalCurve :: OP.Parser (Bool)
optParseDontInterpolateCalCurve = OP.switch (
    OP.long "noInterpolation" <> 
    OP.help "Don't interpolate the calibration curve"
    )

optParseQuiet :: OP.Parser (Bool)
optParseQuiet = OP.switch (
    OP.long "quiet" <> 
    OP.short 'q' <>
    OP.help "Suppress the printing of calibration results to the command line"
    )

optParseDensityFile :: OP.Parser (Maybe FilePath)
optParseDensityFile = OP.option (Just <$> OP.str) (
    OP.long "densityFile" <>
    OP.help "Path to an output file which stores output densities per sample and calender year" <>
    OP.value Nothing
    )

optParseHDRFile :: OP.Parser (Maybe FilePath)
optParseHDRFile = OP.option (Just <$> OP.str) (
    OP.long "hdrFile" <>
    OP.help "Path to an output file which stores the high probability density regions for each \
            \sample" <>
    OP.value Nothing
    )

optParseCalCurveSegmentFile :: OP.Parser (Maybe FilePath)
optParseCalCurveSegmentFile = OP.option (Just <$> OP.str) (
    OP.long "calCurveSegmentFile" <>
    OP.help "Path to an output file which stores the relevant, interpolated calibration curve \
            \segment for the first (!) input date in a long format. \
            \This option as well as --calCurveMatrixFile are mostly meant for debugging" <>
    OP.value Nothing
    )

optParseCalCurveMatrixFile :: OP.Parser (Maybe FilePath)
optParseCalCurveMatrixFile = OP.option (Just <$> OP.str) (
    OP.long "calCurveMatrixFile" <>
    OP.help "Path to an output file which stores the relevant, interpolated calibration curve \
            \segment for the first (!) input date in a wide matrix format" <>
    OP.value Nothing
    )