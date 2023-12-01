module Currycarbon.Utils (
    CurrycarbonException (..),
    renderCurrycarbonException
) where

import           Control.Exception (Exception)

-- | Different exceptions for currycarbon
data CurrycarbonException =
    -- | An exception to describe an issue in the currycarbon CLI input parsing
      CurrycarbonCLIParsingException String
    -- | An exception to describe the case that a date is not in the range of
    -- the supplied calibration curve
    | CurrycarbonCalibrationRangeException String
    -- | An exception for CalPDFs that are unsuitable for certain purposes
    | CurrycarbonInvalidCalPDFException String
    -- | An exception for any issues with the CLI
    | CurrycarbonCLIException String
    deriving (Show)

instance Exception CurrycarbonException

renderCurrycarbonException :: CurrycarbonException -> String
renderCurrycarbonException (CurrycarbonCLIParsingException s) =
    "<!> Error: Input can not be parsed\n" ++ s
renderCurrycarbonException (CurrycarbonCalibrationRangeException i) =
    "<!> Error: Date outside of calibration range. Date ID: " ++ i
renderCurrycarbonException (CurrycarbonInvalidCalPDFException o) =
    "<!> Error: Invalid CalPDF for " ++ o ++
    ", either because all densities are 0 or one density is > 1"
renderCurrycarbonException (CurrycarbonCLIException s) =
    "<!> Error: " ++ s

