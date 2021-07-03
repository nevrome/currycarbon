module Currycarbon.CLI.CLICalibrate where

import           Currycarbon.CalCurves.Intcal20
import           Currycarbon.Calibration
import           Currycarbon.Parsers
import           Currycarbon.Types

import           Control.Monad      (when)
import           Data.Maybe         (fromJust, isJust)
import           System.FilePath    ((</>))
import           System.Directory   (createDirectoryIfMissing)

data CalibrateOptions = CalibrateOptions {
      _calibrateUncalC14 :: [UncalC14],
      _calibrateOutFile :: Maybe FilePath,
      _calibrateExplore :: Bool,
      _calibrateExploreDir :: FilePath
    }

runCalibrate :: CalibrateOptions -> IO ()
runCalibrate (CalibrateOptions uncalC14s outFile explore exploreDir) = do
    -- normal mode
    let calCurve = loadCalCurve intcal20
        calPDFs = calibrateMany calCurve uncalC14s
        calC14 = refineCal calPDFs
    putStrLn $ renderCalC14s calC14
    -- write density file
    when (isJust outFile) $
        writeCalPDFs (fromJust outFile) calPDFs
    -- write all the output for more extensive single date exploration
    when explore $ do
        let (calPDF,calCurveSegment,calCurveMatrix) = calibrateInfo calCurve $ head uncalC14s
        createDirectoryIfMissing True exploreDir
        writeCalCurve (exploreDir </> "calCurveInterpolated.csv") calCurveSegment
        writeCalCurveMatrixFile (exploreDir </> "calCurveMatrix.csv") calCurveMatrix
