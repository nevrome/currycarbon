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
      _uncalC14id :: String -- ^ Identifier, e.g. a lab number
    , _uncalC14age :: Int -- ^ Uncalibrated age BP in years
    , _uncalC14sd :: Int -- ^ One-sigma standard deviation of the uncalibrated age in years
    } deriving Show

-- | A data type to represent a year-wise probability density for uncalibrated dates
-- Although technically not correct, we still call this a probability density function
data UncalPDF = UncalPDF {
      _uncalPDFid :: String -- ^ Identifier, e.g. a lab number
    , _uncalPDFagedens :: V.Vector (Int, Float) -- ^ List that stores years BP and the respective
                                         -- probability densities alongside each other
    } deriving Show

-- | Getter function to access the year BP variable in an uncalibrated age probability density list
getBPsUncal :: UncalPDF -> V.Vector Int
getBPsUncal (UncalPDF _ obs) = V.map fst obs
-- | Getter function to access the density variable in an uncalibrated age probability density list
getProbsUncal :: UncalPDF -> V.Vector Float
getProbsUncal (UncalPDF _ obs) = V.map snd obs

-- | A data type to represent a calibration curve
newtype CalCurve = CalCurve 
    (V.Vector (Int, Int, Int)) -- ^ List that stores the sequence of year BP, 
                      -- year calBP and the one-sigma standard deviation
                      -- (on the year BP scale) that makes up the calibration curve
    deriving Show

-- | Getter function to access the year BP variable in a calibration curve
getBPs :: CalCurve -> V.Vector Int
getBPs (CalCurve obs) = V.map (\(x,_,_) -> x) obs
-- | Getter function to access the year calBP variable in a calibration curve
getCals :: CalCurve -> V.Vector Int
getCals (CalCurve obs) = V.map (\(_,y,_) -> y) obs
-- | Getter function to access the sigma variable in a calibration curve
getCalSigmas :: CalCurve -> V.Vector Int
getCalSigmas (CalCurve obs) = V.map (\(_,_,z) -> z) obs

-- | A data type to represent a calibration curve in a /wide/ matrix form
data CalCurveMatrix = CalCurveMatrix {
      _calCurveMatrixbp :: V.Vector Int -- ^ Row names of the calibration curve matrix:
                                 -- Uncalibrated age BP in years
    , _calCurveMatrixcalbp :: V.Vector Int -- ^ Column names of the calibration curve matrix:
                                    -- Calibrated age BP in years
    , _calCurveMatrixmatrix :: V.Vector (VU.Vector Float) -- ^ Matrix (as a list of columns) with the 
                                         -- probaility densities
    } deriving Show

-- | A data type to represent a year-wise probability density for calibrated ages
data CalPDF = CalPDF {
      _calPDFid :: String -- ^ Identifier, e.g. a lab number
    , _calPDFagedens :: V.Vector (Int, Float) -- ^ List that stores years calBP and the respective
                                         -- probability densities alongside each other
    } deriving Show

-- | Getter function to access the year BP variable in a calibrated age probability density list
getBPsCal :: CalPDF -> V.Vector Int
getBPsCal x = V.map fst $ _calPDFagedens x
-- | Getter function to access the density variable in a calibrated age probability density list
getProbsCal :: CalPDF -> V.Vector Float
getProbsCal x = V.map snd $ _calPDFagedens x

-- | A data type to represent a human readable summary of a calibrated radiocarbon date
data CalC14 = CalC14 {
      _calC14id :: String -- ^ Identifier, e.g. a lab number
    , _calC14hdronesigma :: [HDR] -- ^ One-sigma high density regions
    , _calC14hdrtwosigma :: [HDR] -- ^ Two-sigma high density regions
    } deriving Show

-- | A data type to represent a high density region of a probability distribution.
-- A high density region is here defined as an age range, within which the respective 
-- cummulative probability (e.g. of an calibrated radiocarbon date density curve) 
-- is above a certain threshold
data HDR = HDR {
      _hdrstart :: Int -- ^ Start of the high density region
    , _hdrstop :: Int -- ^ End of the high density region
    } deriving Show
