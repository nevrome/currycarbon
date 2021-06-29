{-# LANGUAGE OverloadedStrings #-}

import           Paths_currycarbon                  (version)
import           Currycarbon.Parsers
import           Currycarbon.Types
import           Currycarbon.CLI.CLICalibrate       (runCalibrate, 
                                                     CalibrateOptions (..))

import           Data.Version                       (showVersion)
import qualified Options.Applicative                as OP

-- data types

data Options = CmdCalibrate CalibrateOptions

-- CLI interface configuration

main :: IO ()
main = do
    cmdOpts <- OP.customExecParser p optParserInfo
    runCmd cmdOpts
    where
        p = OP.prefs OP.showHelpOnEmpty

runCmd :: Options -> IO ()
runCmd o = case o of
    CmdCalibrate opts -> runCalibrate opts

optParserInfo :: OP.ParserInfo Options
optParserInfo = OP.info (OP.helper <*> versionOption <*> optParser) (
    OP.briefDesc <>
    OP.progDesc "currycarbon calibrates C14 dates"
    )

versionOption :: OP.Parser (a -> a)
versionOption = OP.infoOption (showVersion version) (OP.long "version" <> OP.help "Show version")

optParser :: OP.Parser Options
optParser = OP.subparser (
        OP.command "calibrate" calibrateOptInfo
    )
  where
    calibrateOptInfo = OP.info (OP.helper <*> (CmdCalibrate <$> calibrateOptParser))
        (OP.progDesc "Simple intercept calibration for one or multiple radiocarbon dates")

calibrateOptParser :: OP.Parser CalibrateOptions
calibrateOptParser = CalibrateOptions <$> parseUncalC14
                                      <*> parseOutFile
                                      <*> parseExplore
                                      <*> parseExploreDir

parseUncalC14 :: OP.Parser [UncalC14]
parseUncalC14 = OP.argument (OP.eitherReader readUncalC14String) (
    OP.metavar "DATES" <>
    OP.help "A string with one or multiple uncalibrated dates of \
            \ the form \"(<sample name>)<mean age BP>+<one sigma standard deviation>;...\" \
            \ so for example \"(S1)4000+50;3000+25;(S3)1000+20\". \
            \ The sample name is optional."
    )

parseOutFile :: OP.Parser (Maybe FilePath)
parseOutFile = OP.option (Just <$> OP.str) (
    OP.long "outFile" <>
    OP.short 'o' <>
    OP.help "Path to the standard output file which stores the output densities per sample and calender year" <>
    OP.value Nothing
    )

parseExplore :: OP.Parser Bool
parseExplore = OP.switch (
    OP.long "explore" <> 
    OP.help "Produce more (diagnostic) output for the first date in DATES"
    )

parseExploreDir :: OP.Parser FilePath 
parseExploreDir = OP.strOption (
    OP.long "exploreDir" <> 
    OP.help "Output directory for the diagnostic output" <>
    OP.value "currycarbonExplore" <>
    OP.showDefault
    )
