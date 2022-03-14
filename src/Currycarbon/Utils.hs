module Currycarbon.Utils (
    CurrycarbonException (..),
    renderCurrycarbonException
) where

import           Control.Exception      (Exception)

-- | Different exceptions for currycarbon
data CurrycarbonException =
      CurrycarbonCLIParsingException String -- ^ An exception to describe an issue in the currycarbon CLI input parsing
    | CurrycarbonCalibrationRangeException String -- ^ An exection to describe the case that a 
                                                  -- date is not in the range of the supplied calibration curve
    deriving (Show)

instance Exception CurrycarbonException

renderCurrycarbonException :: CurrycarbonException -> String 
renderCurrycarbonException (CurrycarbonCLIParsingException s) = 
    "<!> Error: Input can not be parsed\n" ++ s
renderCurrycarbonException (CurrycarbonCalibrationRangeException s) =
    s ++ " <!> Error: Date outside of calibration range"
