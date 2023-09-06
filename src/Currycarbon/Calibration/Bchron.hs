{-# LANGUAGE Strict #-}

module Currycarbon.Calibration.Bchron (calibrateDateBchron) where

import           Currycarbon.Calibration.Utils
import           Currycarbon.Parsers
import           Currycarbon.Types
import           Currycarbon.Utils

import qualified Data.Vector.Unboxed           as VU

-- | Intercept calibration as implemented in the Bchron R package (see 'Bchron')
calibrateDateBchron :: CalibrationDistribution -> Bool -> Bool -> CalCurveBP -> UncalC14 -> Either CurrycarbonException CalPDF
calibrateDateBchron distr allowOutside interpolate calCurve uncalC14@(UncalC14 name age ageSd) =
    if not allowOutside && isOutsideRangeOfCalCurve calCurve uncalC14
    then Left $ CurrycarbonCalibrationRangeException $ renderUncalC14 uncalC14
    else
        let rawCalCurveSegment = getRelevantCalCurveSegment uncalC14 calCurve
            CalCurveBCAD cals mus tau1s = prepareCalCurveSegment interpolate rawCalCurveSegment
            ageDouble = -(fromIntegral age)+1950
            ageSd2 = ageSd*ageSd
            ageSd2Double = fromIntegral ageSd2
            musDouble = VU.map fromIntegral mus
            tau1sDouble = VU.map fromIntegral tau1s
            dens = case distr of
                NormalDist ->
                    VU.zipWith (\mu tau1 -> dnorm 0 1 ((ageDouble - mu) / sqrt (ageSd2Double + tau1 * tau1))) musDouble tau1sDouble
                StudentTDist degreesOfFreedom ->
                    VU.zipWith (\mu tau1 -> dt degreesOfFreedom ((ageDouble - mu) / sqrt (ageSd2Double + tau1 * tau1))) musDouble tau1sDouble
        in Right $ trimLowDensityEdgesCalPDF $ normalizeCalPDF $ CalPDF name cals dens
