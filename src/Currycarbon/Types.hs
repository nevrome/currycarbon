{-# LANGUAGE StrictData #-}

module Currycarbon.Types where

import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as VU

-- * Data types
--
-- $dataTypes
--
-- This module defines the relevant data types for handling radiocarbon dates

-- | currycarbon includes different calibration algorithms. Currently two distinct
-- implementations are available. Maybe more algorithms will be added in the future.
-- A good default is @Bchron { distribution = StudentTDist 100 }@.
data CalibrationMethod =
  -- | A matrix multiplication method generally following [this blog post by Martin Hinz](https://www.martinhinz.info/jekyll/update/blog/2016/06/03/simple_calibration.html).
  -- This method is slower and the underlying code more verbose than 'Bchron', but it
  -- has some advantages regarding didactics and the inspection of intermediate data
  -- products for debugging.
  -- Using this method is generally not advisable, except for specific applications,
  -- where a more technical insight into C14 calibration is needed
    MatrixMultiplication
  -- | A fast and reliable calibration algorithm very similar to the implementation in the
  -- [R package Bchron by Andrew Parnell](https://github.com/andrewcparnell/Bchron/blob/master/R/BchronCalibrate.R).
  -- This algorithm can be run with a simple normal distribution ('NormalDist') or
  -- Student's t-distribution ('StudentTDist'), which is recommended
  | Bchron { distribution :: CalibrationDistribution }
  deriving (Show, Eq)

-- | A data type to cover the configuration options of the calibrateDates function
data CalibrateDatesConf = CalibrateDatesConf {
      -- | Allow calibration to run outside of the range of the calibration curve
        _calConfAllowOutside                  :: Bool
      -- | Interpolate the calibration curve before calibration.
      -- This is a simple linear interpolation only to increase the output
      -- resolution for earlier time periods, where the typical calibration
      -- curves are less dense by default. With the interpolation, the output
      -- will be a per-year density. The mechanism is inspired by the
      -- [implementation in the Bchron R package](https://github.com/andrewcparnell/Bchron/blob/b202d18550319b488e676a8b542aba55853f6fa3/R/BchronCalibrate.R#L118-L119)
      , _calConfInterpolateCalCurve           :: Bool
      -- | Trim the calibration curve before the calibration.
      -- Reduces the calibration curve to a segment around the mean of the
      -- uncalibrated date +/- six times its standard deviation.
      -- This speeds up calibration.
      , _calConfTrimCalCurveBeforeCalibration :: Bool
      -- | Trim the output CalPDF with a fixed threshold.
      -- Years before or after the first or the last density value of
      -- 0.00001 get removed.
      , _calConfTrimCalPDFAfterCalibration    :: Bool
    } deriving (Show, Eq)

-- | A default configuration that should yield almost identical calibration results
-- to the [Bchron R package](https://github.com/andrewcparnell/Bchron)
defaultCalConf :: CalibrateDatesConf
defaultCalConf = CalibrateDatesConf {
        _calConfAllowOutside = False
      , _calConfInterpolateCalCurve = True
      , _calConfTrimCalCurveBeforeCalibration = True
      , _calConfTrimCalPDFAfterCalibration = True
    }

-- | Statistical distributions to be used with the 'CalibrationMethod' 'Bchron'
data CalibrationDistribution =
  -- | Normal distribution
    NormalDist
  -- | Student's t-distribution.
  | StudentTDist {
      ndf :: Double -- ^ number of degrees of freedom
    }
  deriving (Show, Eq)

-- | A type to represent years BP. All numbers are positive and describe the distance in years
-- to 1950AD: 3000 = 3000BP = 1050BC
type YearBP = Word
-- | A type to represent years BC or AD. Negative values describe years BC, positive values
-- years AD: -5000 = 5000BC and 1300 = 1300AD
type YearBCAD = Int
-- | A type to represent a range of years
type YearRange = Word

-- | A data type to represent an uncalibrated radiocarbon date
data UncalC14 = UncalC14 {
    -- | Sample identifier, e.g. a lab number
      _uncalC14Id    :: String
    -- | C14 age in years BP
    , _uncalC14UnCal :: YearBP
    -- | C14 standard deviation (one sigma in years)
    , _uncalC14Sigma :: YearRange
    } deriving (Show, Eq)

-- | A data type to represent a year-wise probability density for uncalibrated dates
-- Although technically not correct, we still call this a probability density function (PDF)
data UncalPDF = UncalPDF {
    -- | Sample identifier, e.g. a lab number
      _uncalPDFid     :: String
    -- | Years BP
    , _uncalPDFUnCals :: VU.Vector YearBP
    -- | Probability densities
    , _uncalPDFDens   :: VU.Vector Double
    } deriving Show

-- | A data type to represent a calibration curve with 'YearBP'
data CalCurveBP = CalCurveBP {
    -- | Years calBP
      _calCurveBPCals   :: VU.Vector YearBP
    -- | Years BP
    , _calCurveBPUnCals :: VU.Vector YearBP
    -- | Standard deviation (one sigma in years)
    , _calCurveBPSigmas :: VU.Vector YearRange
    } deriving Show

-- | A second data type to represent a calibration curve, here now with 'YearBCAD'
data CalCurveBCAD = CalCurveBCAD {
    -- | Years calBCAD
      _calCurveBCADCals   :: VU.Vector YearBCAD
    -- | Years BCAD
    , _calCurveBCADUnCals :: VU.Vector YearBCAD
    -- | Standard deviation (one sigma in years)
    , _calCurveBCADSigmas :: VU.Vector YearRange
    } deriving Show

-- | A data type to represent a calibration curve in a /wide/ matrix form
data CalCurveMatrix = CalCurveMatrix {
    -- | Row names of the calibration curve matrix: Years BCAD
      _calCurveMatrixUnCals :: VU.Vector YearBCAD
    -- | Column names of the calibration curve matrix: Years calBCAD
    , _calCurveMatrixCals   :: VU.Vector YearBCAD
    -- | Matrix (as a list of columns) with the probability densities
    , _calCurveMatrixDens   :: V.Vector (VU.Vector Double)
    } deriving Show

-- | A data type to represent a year-wise probability density for calibrated dates.
-- Although technically not correct, we still call this a probability density function (PDF)
data CalPDF = CalPDF {
    -- | Sample identifier, e.g. a lab number
      _calPDFid   :: String
    -- | Years calBCAD
    , _calPDFCals :: VU.Vector YearBCAD
    -- | Probability densities for each year in '_calPDFCals'
    , _calPDFDens :: VU.Vector Double
    } deriving (Show, Eq)

-- | A data type for named calibration expressions
data NamedCalExpr = NamedCalExpr {
    -- | Expression identifier
      _exprID :: String
    -- | Expression
    , _expr   :: CalExpr
    } deriving (Show, Eq)

-- | A data type to represent an expression for sum- or product calibration
data CalExpr =
      UnCalDate UncalC14
    | WindowBP TimeWindowBP
    | WindowBCAD TimeWindowBCAD
    | CalDate CalPDF
    | SumCal CalExpr CalExpr
    | ProductCal CalExpr CalExpr
    deriving (Show, Eq)
-- http://www.cse.chalmers.se/edu/year/2018/course/TDA452/lectures/RecursiveDataTypes.html

data TimeWindowBP = TimeWindowBP String YearBP YearBP
    deriving (Show, Eq)

data TimeWindowBCAD = TimeWindowBCAD String YearBCAD YearBCAD
    deriving (Show, Eq)

-- | A data type to represent a human readable summary of a calibrated radiocarbon date
data CalC14 = CalC14 {
    -- | Identifier, e.g. a lab number
      _calC14id           :: String
    -- | Summary of the range of the calibrated date
    , _calC14RangeSummary :: CalRangeSummary
    -- | One-sigma high density regions
    , _calC14HDROneSigma  :: [HDR]
    -- | Two-sigma high density regions
    , _calC14HDRTwoSigma  :: [HDR]
    } deriving Show

-- | A data type to store a simple range summary of the calibrated date, including
-- the median age
data CalRangeSummary = CalRangeSummary {
    -- | Start of the two-sigma age range
      _calRangeStartTwoSigma :: YearBCAD
    -- | Start of the one-sigma age range
    , _calRangeStartOneSigma :: YearBCAD
    -- | Median age
    , _calRangeMedian        :: YearBCAD
    -- | End of the one-sigma age range
    , _calRangeStopOneSigma  :: YearBCAD
    -- | End of the two-sigma age range
    , _calRangeStopTwoSigma  :: YearBCAD
} deriving Show

-- | A data type to represent a high density region of a probability distribution.
-- A high density region is here defined as an age range, within which the respective
-- cumulative probability (e.g. of an calibrated radiocarbon date density curve)
-- is above a certain threshold
data HDR = HDR {
    -- | Start of the high density region in years calBCAD
      _hdrstart :: YearBCAD
    -- | End of the high density region in years calBCAD
    , _hdrstop  :: YearBCAD
    } deriving (Show, Eq)

-- | A data type to store random samples drawn from a calPDF
data RandomAgeSample = RandomAgeSample {
    -- | Identifier
      _rasId      :: String
    -- | Random samples
    , _rasSamples :: VU.Vector YearBCAD
    } deriving Show
