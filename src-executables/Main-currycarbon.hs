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
calibrateOptParser = CalibrateOptions <$> parseC14Age
                                      <*> parseC14Std

parseC14Age :: OP.Parser Int
parseC14Age = OP.option OP.auto (
    OP.short 'c' <>
    OP.long "c14age" <> 
    OP.help "..."
    )

parseC14Std :: OP.Parser Int
parseC14Std = OP.option OP.auto (
    OP.short 's' <>
    OP.long "c14std" <> 
    OP.help "..."
    )
