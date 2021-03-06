{-# LANGUAGE Strict #-}

module Currycarbon.Calibration.Calibration
    ( -- * Calibration functions
      --
      -- $calibration
      --
      -- This module provides an interface to the calibration logic
        getRelevantCalCurveSegment
      , prepareCalCurveSegment
      , makeCalCurveMatrix
      , uncalToPDF
      , calibrateDate
      , calibrateDates
      , refineCalDates
      , refineCalDate
      , CalibrateDatesConf (..)
      , defaultCalConf
    ) where

import Currycarbon.Calibration.Utils
import Currycarbon.Calibration.Bchron
import Currycarbon.Calibration.MatrixMult
import Currycarbon.Types
import Currycarbon.Utils

import Data.List (sort, sortBy, groupBy, elemIndex)
import Data.Maybe (fromJust)
import qualified Data.Vector.Unboxed as VU

-- | A data type to cover the configuration options of the calibrateDates function
data CalibrateDatesConf = CalibrateDatesConf {
      -- | The calibration algorithm that should be used  
        _calConfMethod :: CalibrationMethod
      -- | Allow calibration to run outside of the range of the calibration curve
      , _calConfAllowOutside :: Bool
      -- | Interpolate the calibration curve before calibration.
      -- This is a simple linear interpolation only to increase the output
      -- resolution for earlier time periods, where the typical calibration
      -- curves are less dense by default. With the interpolation, the output
      -- will be a per-year density. The mechanism is inspired by the 
      -- [implementation in the Bchron R package](https://github.com/andrewcparnell/Bchron/blob/b202d18550319b488e676a8b542aba55853f6fa3/R/BchronCalibrate.R#L118-L119)
      , _calConfInterpolateCalCurve :: Bool 
    } deriving (Show, Eq)

-- | A default configuration that should yield almost identical calibration results 
-- to the [Bchron R package](https://github.com/andrewcparnell/Bchron)
defaultCalConf :: CalibrateDatesConf
defaultCalConf = CalibrateDatesConf {
        _calConfMethod = Bchron { distribution = StudentTDist 100 }
      , _calConfAllowOutside = False 
      , _calConfInterpolateCalCurve = True
    }

-- | Calibrates a list of dates with the provided calibration curve
calibrateDates :: CalibrateDatesConf -- ^ Configuration options to consider
                  -> CalCurveBP -- ^ A calibration curve
                  -> [UncalC14] -- ^ A list of uncalibrated radiocarbon dates  
                  -> [Either CurrycarbonException CalPDF] -- ^ The function returns a list for each input date, with
                                                          -- either an exception if the calibration failed for some
                                                          -- reason, or a 'CalPDF'
calibrateDates _ _ [] = []
calibrateDates (CalibrateDatesConf MatrixMultiplication allowOutside interpolate) calCurve uncalDates =
    map (calibrateDateMatrixMult allowOutside interpolate calCurve) uncalDates
calibrateDates (CalibrateDatesConf Bchron{distribution=distr} allowOutside interpolate) calCurve uncalDates =
    map (calibrateDateBchron distr allowOutside interpolate calCurve) uncalDates

-- | Calibrates a date with the provided calibration curve
calibrateDate :: CalibrateDatesConf -- ^ Configuration options to consider
                 -> CalCurveBP -- ^ A calibration curve
                 -> UncalC14 -- ^ An uncalibrated radiocarbon date
                 -> Either CurrycarbonException CalPDF -- ^ The function returns either an exception if the 
                                                        -- calibration failed for some reason, or a 'CalPDF'
calibrateDate (CalibrateDatesConf MatrixMultiplication allowOutside interpolate) calCurve uncalDate =
    calibrateDateMatrixMult allowOutside interpolate calCurve uncalDate
calibrateDate (CalibrateDatesConf Bchron{distribution=distr} allowOutside interpolate) calCurve uncalDate =
    calibrateDateBchron distr allowOutside interpolate calCurve uncalDate

-- | Transforms the raw, calibrated probability density table to a meaningful representation of a
-- calibrated radiocarbon date
refineCalDates :: [CalPDF] -> [Maybe CalC14]
refineCalDates = map refineCalDate

refineCalDate :: CalPDF -> Maybe CalC14
refineCalDate (CalPDF name cals dens) =
    if VU.sum dens == 0 || VU.length (VU.filter (>= 1.0) dens) == 1 -- don't calculate CalC14, if it's not meaningful
    then Nothing
    else Just $ CalC14 {
          _calC14id           = name
        , _calC14RangeSummary = CalRangeSummary {
              _calRangeStartTwoSigma = _hdrstart $ head hdrs95
            , _calRangeStartOneSigma = _hdrstart $ head hdrs68
            , _calRangeMedian        = fromJust $ cals `indexVU` elemIndex (minimum distanceTo05) distanceTo05
            , _calRangeStopOneSigma  = _hdrstop  $ last hdrs68
            , _calRangeStopTwoSigma  = _hdrstop  $ last hdrs95
            }
        , _calC14HDROneSigma  = hdrs68
        , _calC14HDRTwoSigma  = hdrs95
    }
    where
        -- simple density cumsum for median age
        cumsumDensities = cumsumDens (VU.toList $ VU.zip cals dens)
        distanceTo05 = map (\x -> abs $ (x - 0.5)) cumsumDensities
        -- sorted density cumsum for hdrs
        sortedDensities = sortBy (flip (\ (_, dens1) (_, dens2) -> compare dens1 dens2)) (VU.toList $ VU.zip cals dens)
        cumsumSortedDensities = cumsumDens sortedDensities
        isIn68 = map (< 0.683) cumsumSortedDensities
        isIn95 = map (< 0.954) cumsumSortedDensities
        contextualizedDensities = sort $ zipWith3 (\(y,d) in68 in95 -> (y,d,in68,in95)) sortedDensities isIn68 isIn95
        hdrs68 = densities2HDR68 contextualizedDensities
        hdrs95 = densities2HDR95 contextualizedDensities
        -- helper functions
        indexVU _ Nothing = Nothing
        indexVU x (Just i) = x VU.!? i
        cumsumDens :: [(YearBCAD, Float)] -> [Float]
        cumsumDens x = scanl1 (+) $ map snd x
        densities2HDR68 :: [(Int, Float, Bool, Bool)] -> [HDR]
        densities2HDR68 cDensities = 
            let highDensityGroups = groupBy (\(_,_,in681,_) (_,_,in682,_) -> in681 == in682) cDensities
                filteredDensityGroups = filter (all getIn68) highDensityGroups
            in map (\xs -> let yearRange = map getYear xs in HDR (head yearRange) (last yearRange)) filteredDensityGroups
        densities2HDR95 :: [(Int, Float, Bool, Bool)] -> [HDR]
        densities2HDR95 cDensities = 
            let highDensityGroups = groupBy (\(_,_,_,in951) (_,_,_,in952) -> in951 == in952) cDensities
                filteredDensityGroups = filter (all getIn95) highDensityGroups
            in map (\xs -> let yearRange = map getYear xs in HDR (head yearRange) (last yearRange)) filteredDensityGroups
        getIn68 :: (Int, Float, Bool, Bool) -> Bool
        getIn68 (_,_,x,_) = x
        getIn95 :: (Int, Float, Bool, Bool) -> Bool
        getIn95 (_,_,_,x) = x
        getYear :: (Int, Float, Bool, Bool) -> Int
        getYear (year,_,_,_) = year
