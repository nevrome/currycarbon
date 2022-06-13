-- | This module implements an algorithm for the calibration of 
-- [radiocarbon dates](https://en.wikipedia.org/wiki/Radiocarbon_dating).
-- This is a standard procedure in Archaeology and other fields working
-- with radiocarbon dating.

module Currycarbon (
    
    -- * Calibration
    -- $calibration
    calibrateDates,
    -- ** Configuration
    CalibrateDatesConf (..),
    defaultCalConf,
    CalibrationMethod (..),
    -- ** Input
    UncalC14 (..),
    readUncalC14FromFile,
    -- ** Output
    CalPDF (..),
    writeCalPDFs,

    -- * Year data types
    -- $yearDataTypes
    YearBP,
    YearBCAD,
    YearRange,

    -- * Calibration curves
    -- $calCurves
    CalCurveBP (..),
    CalCurveBCAD (..),
    intcal20,
    readCalCurveFromFile,

    -- * Derived output
    -- $derivedOutput
    refineCalDates,
    CalC14 (..),
    writeCalC14s,
    renderCalDatePretty
    ) where

import Currycarbon.Calibration.Calibration
import Currycarbon.Parsers
import Currycarbon.SumCalibration
import Currycarbon.Types
import Currycarbon.CalCurves.Intcal20

{- $calibration

The main function in this module 'calibrateDates' calibrates 
radiocarbon dates, given the uncalibrated input dates, a calibration 
curve and some configuration options. 

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

{- $yearDataTypes

A number of types were introcuded to distinguish clearly between
ages in years BP, years BC/AD and year ranges (e.g. for standard
deviations). Generally currycarbon handles input ('UncalC14',
'UncalPDF') with 'YearBP', and output ('CalPDF', 'CalC14') with
'YearBCAD'. The switch happens as part of the the calibration
process, so that calibration curves have to be adjusted as well.
That is why the two types 'CalCurveBP' and 'CalCurveBCAD' are
distinguished.
-}

{- $calCurves

Currycarbon features two separate data types for calibration curves:
'CalCurveBP' and 'CalCurveBCAD' to distinguish between versions with
'YearBP' and 'YearBCAD'.

The library only comes with one curve: 'intcal20'. At runtime curves
can be read and used with 'readCalCurveFromFile'.
-}

{- $derivedOutput

The main calibration function 'calibrateDates' returns a list of
'CalPDF's. This is very useful output for further computational
analysis, but it is not optimised for human reading and understanding.
'refineCalDates' therefore takes these probability distributions and
turns them into the derived data type 'CalC14', which features high
density regions ('HDR's). HDRs are the age ranges a sample most likely
dates to according to the post calibration probability distribution.

These can also be written to a file with 'writeCalC14s'.

'renderCalDatesPretty' finally combines 'UncalC14', 'CalPDF' and 
'CalC14' to produce nice command line output summarising the calibration
result for a given sample.
-}