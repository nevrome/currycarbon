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
    -- | An exception for issues with the .tsv file parsing
    | CurrycarbonTSVParsingException String
    -- | An exception for incomplete rows in .tsv files
    | CurrycarbonTSV2CalExprException String
    -- | An exception for issues when making CalEXPRs
    | CurrycarbonMakeCalExprException String
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
renderCurrycarbonException (CurrycarbonTSVParsingException s) =
    "<!> Error: " ++ s
renderCurrycarbonException (CurrycarbonTSV2CalExprException i) =
    "<!> Error: Incomplete information in .tsv entry " ++ show i
renderCurrycarbonException (CurrycarbonMakeCalExprException s) =
    "<!> Error: Issue when making CalExpr: " ++ s
