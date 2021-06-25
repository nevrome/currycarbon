{-# LANGUAGE OverloadedStrings #-}

import           Paths_currycarbon                   (version)
import           Currycarbon.Parsers
import           Currycarbon.Types
import           Currycarbon.CLI.Calibrate           (runCalibrate, 
                                                 CalibrateOptions (..))

import           Data.Version                   (showVersion)
import qualified Options.Applicative            as OP

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
    OP.progDesc "currycarbon calibrates C14 dates."
    )

versionOption :: OP.Parser (a -> a)
versionOption = OP.infoOption (showVersion version) (OP.long "version" <> OP.help "Show version")

optParser :: OP.Parser Options
optParser = OP.subparser (
        OP.command "calibrate" calibrateOptInfo
    )
  where
    calibrateOptInfo = OP.info (OP.helper <*> (CmdCalibrate <$> calibrateOptParser))
        (OP.progDesc "...")

calibrateOptParser :: OP.Parser CalibrateOptions
calibrateOptParser = CalibrateOptions <$> parseUncalC14
                                      <*> parseShowPlots
                                      <*> parseOutFile

parseUncalC14 :: OP.Parser [UncalC14]
parseUncalC14 = OP.argument (OP.eitherReader readUncalC14String) (
    OP.metavar "DATES" <>
    OP.help "\"S1:4000+50;S2:3000+25;S3:1000+20\""
    )

parseShowPlots :: OP.Parser Bool
parseShowPlots = OP.switch (
    OP.long "showPlots" <> 
    OP.help "Show plots"
    )

parseOutFile :: OP.Parser FilePath
parseOutFile = OP.strOption (
    OP.long "outFile" <>
    OP.short 'o' <>
    OP.help "The output file path"
    )
