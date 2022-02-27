-- | This module implements an algorithm for the calibration of 
-- [radiocarbon dates](https://en.wikipedia.org/wiki/Radiocarbon_dating).
-- This is a standard procedure in Archaeology and other fields working
-- with radiocarbon dating.

module Currycarbon (
    
    -- * Calibration
    -- $calibration
    calibrateDates,
    UncalC14 (..),
    readUncalC14FromFile,
    CalibrateDatesConf (..),
    defaultCalConf,
    CalibrationMethod (..),
    CalPDF (..),
    writeCalPDFs,

    -- * Calibration curves
    CalCurve (..),
    intcal20,
    readCalCurveFromFile,

    -- * Derived output
    -- $derivedOutput
    refineCalDates,
    CalC14 (..),
    renderCalDatesPretty,
    writeCalC14s

    ) where

import Currycarbon.Calibration.Calibration
import Currycarbon.Parsers
import Currycarbon.Types
import Currycarbon.CalCurves.Intcal20

{- $calibration

The main function 'calibrateDates'  in this module calibrates radiocarbon 
dates, given the uncalibrated input dates, a calibration curve and some 
configuration options. 

* For the input dates there is a dedicated data type 'UncalC14'.
These can be read from a .csv file with 'readUncalC14FromFile'.

* Calibration curves are covered with the data type 'Calcurve'.
Only one curve is embedded in the package ('incal20'), others
can be read at runtime with 'readCalCurveFromFile'.

* The configuration options are managed in 'CalibrateDatesConf',
within which 'CalibrationMethod' is most important. For a solid
default I suggest to use 'defaultCalConf'.

'calibrateDates' returns a list of calibrated dates in the rough
'CalPDF' format, which can be written to a file with 'writeCalPDFs'.
See the Derived output section below for more pretty output formats.
-}

-- $derivedOutput