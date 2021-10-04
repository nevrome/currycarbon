module Currycarbon.Utils (
    CurrycarbonException (..),
    renderCurrycarbonException
) where

import           Control.Exception (Exception)

-- | Different exceptions for currycarbon
data CurrycarbonException =
      CurrycarbonCLIParsingException String -- ^ An exception to describe an issue in the currycarbon CLI input parsing
    | CurrycarbonCalibrationRangeException String -- ^ An exection to describe the case that a 
                                                  -- date is not in the range of the supplied calibration curve
    deriving (Show)

instance Exception CurrycarbonException

renderCurrycarbonException :: CurrycarbonException -> String 
renderCurrycarbonException (CurrycarbonCLIParsingException s) = 
    "/!\\ Issue when parsing the input: " ++ s
renderCurrycarbonException (CurrycarbonCalibrationRangeException id) = 
    "/!\\ Sample: " ++ id ++ "\nOutside of calibration range. It will be ignored."
