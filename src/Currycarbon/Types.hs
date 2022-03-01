{-# LANGUAGE StrictData #-}

module Currycarbon.Types where

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import Numeric.Natural (Natural)

-- * Data types
--
-- $dataTypes
--
-- This module defines the relevant data types for handling radiocarbon dates

-- | Different calibration algorithms implemented in currycarbon. Currently two distinct 
-- implementations are available, although both of them are similar [Intercept calibration](https://en.wikipedia.org/wiki/Radiocarbon_calibration#Intercept)
-- algorithms. Maybe more algorithms will be added in the future
data CalibrationMethod =
  -- | A matrix multiplication method generally following [this blog post by Martin Hinz](https://www.martinhinz.info/jekyll/update/blog/2016/06/03/simple_calibration.html).
  -- This method is slower and the underlying code more verbose than 'Bchron', but it 
  -- has some advantages regarding didactics and the inspection of intermediate data
  -- products for debugging.
  -- Using this method is thus generally not advisable, except for specific applications,
  -- where a more technical insight into C14 calibration is needed
    MatrixMultiplication
  -- | A fast and reliable calibration algorithm very similar to the implementation in the 
  -- [R package Bchron by Andrew Parnell](https://github.com/andrewcparnell/Bchron/blob/master/R/BchronCalibrate.R).
  -- This algorithm can be run with a simple normal distribution ('NormalDist') or
  -- Student's t-distribution ('StudentTDist'), which is recommended
  | Bchron { distribution :: CalibrationDistribution }
  deriving (Show, Eq)

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
      _uncalC14Id :: String -- ^ Sample identifier, e.g. a lab number
    , _uncalC14BP :: YearBP -- ^ C14 age in years BP
    , _uncalC14Sigma :: YearRange -- ^ C14 standard deviation (one sigma in years)
    } deriving (Show, Eq)

-- | A data type to represent a year-wise probability density for uncalibrated dates
-- Although technically not correct, we still call this a probability density function (PDF)
data UncalPDF = UncalPDF {
      _uncalPDFid :: String -- ^ Sample identifier, e.g. a lab number
    , _uncalPDFBPs :: VU.Vector YearBP -- ^ Years BP
    , _uncalPDFDens :: VU.Vector Float -- ^ Probability densities
    } deriving Show

-- | A data type to represent a calibration curve
data CalCurveBP = CalCurveBP {
      _calCurveBPCals :: VU.Vector YearBP -- ^ Years calBP
    , _calCurveBPUnCals :: VU.Vector YearBP -- ^ Years BP
    , _calCurveBPSigmas :: VU.Vector YearRange -- ^ Standard deviation (one sigma in years)
    } deriving Show

data CalCurveBCAD = CalCurveBCAD {
      _calCurveBCADCals :: VU.Vector YearBCAD -- ^ Years calBCAD
    , _calCurveBCADUnCals :: VU.Vector YearBCAD -- ^ Years BCAD
    , _calCurveBCADSigmas :: VU.Vector YearRange -- ^ Standard deviation (one sigma in years)
    } deriving Show

-- | A data type to represent a calibration curve in a /wide/ matrix form
data CalCurveMatrix = CalCurveMatrix {
      _calCurveMatrixBCADs :: VU.Vector YearBCAD -- ^ Row names of the calibration curve matrix: Years BCAD
    , _calCurveMatrixCalBCADs :: VU.Vector YearBCAD -- ^ Column names of the calibration curve matrix: Years calBCAD
    , _calCurveMatrixDens :: V.Vector (VU.Vector Float) -- ^ Matrix (as a list of columns) with the probability densities
    } deriving Show

-- | A data type to represent a year-wise probability density for calibrated dates.
-- Although technically not correct, we still call this a probability density function (PDF)
data CalPDF = CalPDF {
      _calPDFid :: String -- ^ Sample identifier, e.g. a lab number
    , _calPDFBCADs :: VU.Vector YearBCAD -- ^ Years calBCAD
    , _calPDFDens :: VU.Vector Float -- ^ Probability densities for each year in '_calPDFBCADs'
    } deriving Show

-- | A data type to represent a human readable summary of a calibrated radiocarbon date
data CalC14 = CalC14 {
      _calC14id :: String -- ^ Identifier, e.g. a lab number
    , _calC14HDROneSigma :: [HDR] -- ^ One-sigma high density regions
    , _calC14HDRTwoSigma :: [HDR] -- ^ Two-sigma high density regions
    } deriving Show

-- | A data type to represent a high density region of a probability distribution.
-- A high density region is here defined as an age range, within which the respective 
-- cummulative probability (e.g. of an calibrated radiocarbon date density curve) 
-- is above a certain threshold
data HDR = HDR {
      _hdrstart :: YearBCAD -- ^ Start of the high density region in years calBCAD
    , _hdrstop :: YearBCAD -- ^ End of the high density region in years calBCAD
    } deriving (Show, Eq)
