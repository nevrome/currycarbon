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
      , calibrateDates
      , refineCalDates
    ) where

import Currycarbon.Calibration.Utils
import Currycarbon.Calibration.Bchron
import Currycarbon.Calibration.MatrixMult
import Currycarbon.Types
import Currycarbon.Utils

import Control.Parallel.Strategies (parListChunk, using, rpar)
import Data.List (sort, sortBy, groupBy)
import qualified Data.Vector.Unboxed as VU

--import Statistics.Distribution.Normal (normalDistr)

-- | Calibrates a list of dates with the provided calibration curve
calibrateDates :: CalibrationMethod -- ^ Calibration method
                  -> Bool -- ^ Should calibration be allowed to run outside of the range of the calibration curve? 
                  -> Bool -- ^ Should the calibration curve be interpolated for year-wise output?
                  -> CalCurve -- ^ Calibration curve
                  -> [UncalC14] -- ^ A list of uncalibrated radiocarbon dates
                  -> [Either CurrycarbonException CalPDF]
calibrateDates _ _ _ _ [] = []
calibrateDates MatrixMultiplication allowOutside interpolate calCurve uncalDates =
    map (calibrateDateMatrixMult allowOutside interpolate calCurve) uncalDates `using` parListChunk 20 rpar
calibrateDates Bchron{distribution=distr} allowOutside interpolate calCurve uncalDates =
    map (calibrateDateBchron distr allowOutside interpolate calCurve) uncalDates `using` parListChunk 100 rpar
-- TODO: the chunking into 20/100 elements is arbitrary and requires solid testing
-- Maybe a special condition for less than X dates is useful

-- | Transforms the raw, calibrated probability density table to a meaningful representation of a
-- calibrated radiocarbon date
refineCalDates :: [CalPDF] -> [CalC14]
refineCalDates = map refineCalDate

refineCalDate :: CalPDF -> CalC14
refineCalDate (CalPDF name bps dens) =
    let sortedDensities = sortBy (flip (\ (_, dens1) (_, dens2) -> compare dens1 dens2)) (VU.toList $ VU.zip bps dens)
        cumsumDensities = scanl1 (+) $ map snd sortedDensities
        isIn68 = map (< 0.683) cumsumDensities
        isIn95 = map (< 0.954) cumsumDensities
        contextualizedDensities = reverse $ sort $ zipWith3 (\(y,d) in68 in95 -> (y,d,in68,in95)) sortedDensities isIn68 isIn95
    in CalC14 name (densities2HDR68 contextualizedDensities) (densities2HDR95 contextualizedDensities)
    where
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
