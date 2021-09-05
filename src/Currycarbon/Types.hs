{-# LANGUAGE StrictData #-}

module Currycarbon.Types where

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V

-- * Data types
--
-- $dataTypes
--
-- This module defines the relevant data types for handling radiocarbon dates

-- | A data type to represent an uncalibrated radiocarbon date
data UncalC14 = UncalC14 {   
      _uncalC14Id :: String -- ^ Identifier, e.g. a lab number
    , _uncalC14BP :: Int -- ^ Years BP
    , _uncalC14Sigma :: Int -- ^ Standard deviation (one sigma in years)
    } deriving Show

-- | A data type to represent a year-wise probability density for uncalibrated dates
-- Although technically not correct, we still call this a probability density function (PDF)
data UncalPDF = UncalPDF {
      _uncalPDFid :: String -- ^ Identifier, e.g. a lab number
    , _uncalPDFBPs :: VU.Vector Int -- ^ Years BP or BC
    , _uncalPDFDens :: VU.Vector Float -- ^ Probability densities
    } deriving Show

-- | A data type to represent a calibration curve
data CalCurve = CalCurve {
      _calCurveBPs :: VU.Vector Int -- ^ Years BP or BC
    , _calCurveCals :: VU.Vector Int -- ^ Years calBP or calBC
    , _calCurveSigmas :: VU.Vector Int -- ^ Standard deviation (one sigma in years)
    } deriving Show

-- | A data type to represent a calibration curve in a /wide/ matrix form
data CalCurveMatrix = CalCurveMatrix {
      _calCurveMatrixBPs :: VU.Vector Int -- ^ Row names of the calibration curve matrix: Years BP or BC
    , _calCurveMatrixCalBPs :: VU.Vector Int -- ^ Column names of the calibration curve matrix: Years calBP or calBC
    , _calCurveMatrixDens :: V.Vector (VU.Vector Float) -- ^ Matrix (as a list of columns) with the probability densities
    } deriving Show

-- | A data type to represent a year-wise probability density for calibrated ages
data CalPDF = CalPDF {
      _calPDFid :: String -- ^ Identifier, e.g. a lab number
    , _calPDFBPs :: VU.Vector Int -- ^ Years calBP or calBC
    , _calPDFDens :: VU.Vector Float -- ^ Probability densities
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
      _hdrstart :: Int -- ^ Start of the high density region
    , _hdrstop :: Int -- ^ End of the high density region
    } deriving Show
