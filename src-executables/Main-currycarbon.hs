{-# LANGUAGE OverloadedStrings #-}

import           Currycarbon.CLI.RunCalibrate (CalibrateOptions (..),
                                               runCalibrate)
import           Currycarbon.Parsers
import           Currycarbon.Types
import           Currycarbon.Utils
import           Paths_currycarbon            (version)
import Currycarbon.CalCurves

import           Control.Exception            (catch)
import           Data.Version                 (showVersion)
import qualified Options.Applicative          as OP
import qualified Options.Applicative.Help     as OH
import           System.Exit                  (exitFailure)
import           System.IO                    (hGetEncoding, hPutStrLn, stderr,
                                               stdout)
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
calibrateOptParser = CalibrateOptions <$> optParseNamedCalExprString
                                      <*> optParseNamedCalExprFromFile
                                      <*> optParseCalCurveSelection
                                      <*> optParseCalibrationMethod
                                      <*> optParseAllowOutside
                                      <*> optParseDontInterpolateCalCurve
                                      <*> optParseDontTrimCalCurve
                                      <*> optParseDontTrimOutCalPDF
                                      <*> optParseQuiet
                                      <*> pure "unknown"
                                      <*> optParseBasicFile
                                      <*> optParseDensityFile
                                      <*> optParseHDRFile
                                      <*> optParseAgeSamplingSettings
                                      <*> optParseCalCurveSegmentFile
                                      <*> optParseCalCurveMatrixFile

-- ** Input parsing functions
--
-- $inputParsing
--
-- These functions define and handle the CLI input arguments

optParseNamedCalExprString :: OP.Parser [NamedCalExpr]
optParseNamedCalExprString = concat <$> OP.many (OP.argument (OP.eitherReader readNamedCalExprs) (
    OP.metavar "CalEXPRs" <>
    OP.helpDoc ( Just (
        "---"
        <> OH.hardline
        <> s2d "A string to specify \"calibration expressions\", so small chronological \
           \models for individual events. These can include uncalibrated radiocarbon ages, \
           \uniform age ranges and operations to combine the resulting age probability \
           \distribution as sums or products."
        <> OH.hardline <>
            s2d "The expression language includes the following functions:"
        <> OH.hardline
        <> OH.hardline <> "- calExpr(id = STRING, expr = EXPR)"
        <> OH.hardline <> "- uncalC14(id = STRING, yearBP = INT, sigma = INT)"
        <> OH.hardline <> "- rangeBP(id = STRING, start = INT, stop = INT)"
        <> OH.hardline <> "- rangeBCAD(id = STRING, start = INT, stop = INT)"
        <> OH.hardline <> "- sum(a = EXPR, b = EXPR)"
        <> OH.hardline <> "- product(a = EXPR, b = EXPR)"
        <> OH.hardline
        <> OH.hardline
        <> s2d "The order of arguments is fixed, but the argument names '<arg> =' \
           \can be left out. The 'id' arguments are optional. \
           \Some functions can be shortened with syntactic sugar:"
        <> OH.hardline
        <> OH.hardline <> "- calExpr(STRING, EXPR) -> id: EXPR"
        <> OH.hardline <> "- uncalC14(STRING, INT, INT) -> STRING,INT,INT"
        <> OH.hardline <> "- sum(EXPR, EXPR) -> EXPR + EXPR"
        <> OH.hardline <> "- product(EXPR, EXPR) -> EXPR * EXPR"
        <> OH.hardline
        <> OH.hardline
        <> s2d "Parentheses '()' can be used to specify the evaluation order within \
           \an expression. Multiple expressions can be chained, separated by ';'."
        <> OH.hardline
        <> OH.hardline
        <> "Examples:"
        <> OH.hardline <> s2d "1. Calibrate a single radiocarbon date with a mean age BP \
                          \and a one sigma standard deviation:"
        <> OH.hardline <> "\"3000,30\" or \"uncalC14(yearBP = 3000, sigma = 30)\""
        <> OH.hardline <> s2d "2. Calibrate two radiocarbon dates and sum them:"
        <> OH.hardline <> "\"(3000,30) + (3100,40)\" or"
        <> OH.hardline <> "\"sum(uncalC14(3000,30), uncalC14(3100,40))\""
        <> OH.hardline <> s2d "3. Compile a complex, named expression:"
        <> OH.hardline <> "\"Ex3: ((3000,30) + (3100,40)) * rangeBP(3200,3000)\""
        <> OH.hardline
        <> "---"
    ))
    ))

s2d :: String -> OH.Doc
s2d str = OH.fillSep $ map OH.pretty $ words str

optParseNamedCalExprFromFile :: OP.Parser [FilePath]
optParseNamedCalExprFromFile = OP.many (OP.strOption (
    OP.long "inputFile" <>
    OP.short 'i' <>
    OP.metavar "FILE" <>
    OP.help "A file with a list of calibration expressions. \
            \Formatted just as CalEXPRs, but with a new line for each input expression. \
            \CalEXPRs and --inputFile can be combined and you can provide multiple \
            \instances of --inputFile. \
            \Note that syntactic sugar allows to read simple radiocarbon dates from \
            \a headless .csv file with one sample per row: \
            \<sample name>,<mean age BP>,<one sigma standard deviation>."
    ))

optParseCalCurveSelection :: OP.Parser CalCurveSelection
optParseCalCurveSelection = OP.option (OP.eitherReader readCalCurveSelection) (
    OP.long "calCurve" <>
    OP.metavar "IntCal20 | SHCal20 | Marine20 | FILE" <>
    OP.help "Either one of the included calibration curves, or a \
            \file path to an calibration curve file in '.14c' format. \
            \The calibration curve will be read and used for calibration." <>
    OP.value IntCal20 <>
    OP.showDefault
    )

optParseCalibrationMethod :: OP.Parser CalibrationMethod
optParseCalibrationMethod = OP.option (OP.eitherReader readCalibrationMethod) (
    OP.long "method" <>
    OP.metavar "DSL" <>
    OP.helpDoc ( Just (
            s2d "The calibration algorithm that should be used: \
            \'<Method>,<Distribution>,<NumberOfDegreesOfFreedom>'. "
        <> OH.hardline <>
            s2d "The default setting is equivalent to \"Bchron,StudentT,100\" \
            \which copies the algorithm implemented in the Bchron R package. \
            \For the Bchron algorithm with a normal distribution (\"Bchron,Normal\") \
            \the degrees of freedom argument is not relevant"
        <> OH.hardline <>
            s2d "Alternatively we implemented  \"MatrixMult\", which comes without further \
            \arguments."
    )) <>
    OP.value (Bchron $ StudentTDist 100)
    )

optParseAllowOutside :: OP.Parser Bool
optParseAllowOutside = OP.switch (
    OP.long "allowOutside" <>
    OP.help "Allow calibrations to run outside the range of the calibration curve."
    )

optParseDontInterpolateCalCurve :: OP.Parser Bool
optParseDontInterpolateCalCurve = OP.switch (
    OP.long "noInterpolation" <>
    OP.help "Do not interpolate the calibration curve."
    )

optParseDontTrimCalCurve :: OP.Parser Bool
optParseDontTrimCalCurve = OP.switch (
    OP.long "noTrimCalCurve" <>
    OP.help "Do not trim the calibration curve before the calibration. \
            \If a probability distribution over the entire range \
            \of the calibration curve is needed. See also --noTrimOutCalPDF."
    )

optParseDontTrimOutCalPDF :: OP.Parser Bool
optParseDontTrimOutCalPDF = OP.switch (
    OP.long "noTrimOutCalPDF" <>
    OP.help "Do not trim the output CalPDF. \
            \If an untrimmed probability distribution is needed. \
            \See also --noTrimCalCurve."
    )

optParseQuiet :: OP.Parser Bool
optParseQuiet = OP.switch (
    OP.long "quiet" <>
    OP.short 'q' <>
    OP.help "Suppress the printing of calibration results to the command line."
    )

optParseBasicFile :: OP.Parser (Maybe FilePath)
optParseBasicFile = OP.option (Just <$> OP.str) (
    OP.long "basicFile" <>
    OP.metavar "FILE" <>
    OP.help "Path to an output file to store basic, per-expression output: \
            \The minimum start and maximum end of \
            \the high probability density regions and the median age." <>
    OP.value Nothing
    )

optParseDensityFile :: OP.Parser (Maybe FilePath)
optParseDensityFile = OP.option (Just <$> OP.str) (
    OP.long "densityFile" <>
    OP.metavar "FILE" <>
    OP.help "Path to an output file to store output densities per CalEXPR and calender \
            \year." <>
    OP.value Nothing
    )

optParseHDRFile :: OP.Parser (Maybe FilePath)
optParseHDRFile = OP.option (Just <$> OP.str) (
    OP.long "hdrFile" <>
    OP.metavar "FILE" <>
    OP.help "Path to an output file to store the high probability density regions for each \
            \CalEXPR." <>
    OP.value Nothing
    )

optParseAgeSamplingSettings :: OP.Parser (Maybe (Maybe Word, Word, FilePath))
optParseAgeSamplingSettings =
    OP.optional $ (,,) <$>
            optParseAgeSamplingConfSeed
        <*> optParseAgeSamplingConfNrOfSamples
        <*> optParseAgeSamplingFile

optParseAgeSamplingConfSeed :: OP.Parser (Maybe Word)
optParseAgeSamplingConfSeed = OP.option (Just <$> OP.auto) (
       OP.long  "seed"
    <> OP.metavar "INT"
    <> OP.help  "Seed for the random number generator for age sampling. \
                \The default causes currycarbon to fall back to a random seed."
    <> OP.value Nothing
    <> OP.showDefault
    )

optParseAgeSamplingConfNrOfSamples :: OP.Parser Word
optParseAgeSamplingConfNrOfSamples = OP.option OP.auto (
       OP.short 'n'
    <> OP.long "nrSamples"
    <> OP.metavar "INT"
    <> OP.help "Number of age samples to draw per CalEXPR."
    )

optParseAgeSamplingFile :: OP.Parser FilePath
optParseAgeSamplingFile = OP.strOption (
    OP.long "samplesFile" <>
    OP.metavar "FILE" <>
    OP.help "Path to an output file to store age samples for each CalEXPR."
    )

optParseCalCurveSegmentFile :: OP.Parser (Maybe FilePath)
optParseCalCurveSegmentFile = OP.option (Just <$> OP.str) (
    OP.long "calCurveSegFile" <>
    OP.metavar "FILE" <>
    OP.help "Path to an output file to store the relevant, interpolated calibration curve \
            \segment for the first (!) input date. \
            \This option as well as --calCurveMatFile are meant for debugging." <>
    OP.value Nothing
    )

optParseCalCurveMatrixFile :: OP.Parser (Maybe FilePath)
optParseCalCurveMatrixFile = OP.option (Just <$> OP.str) (
    OP.long "calCurveMatFile" <>
    OP.metavar "FILE" <>
    OP.help "Path to an output file which stores the relevant, interpolated calibration curve \
            \segment for the first (!) input date in a wide matrix format." <>
    OP.value Nothing
    )
