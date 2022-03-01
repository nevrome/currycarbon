{-# LANGUAGE Strict #-}

module Currycarbon.Calibration.Bchron (calibrateDateBchron) where


import Currycarbon.CalCurves.Intcal20 -- only for the doctest
import Currycarbon.Calibration.Utils
import Currycarbon.Types
import Currycarbon.Utils

import qualified Data.Vector.Unboxed as VU

-- | Intercept calibration as implemented in the Bchron R package (see 'Bchron')
--
-- >>> :{
--   let calResult = calibrateDateBchron NormalDist False True intcal20 (UncalC14 "" 3000 30)
--       simplifyResult = \(Right (CalPDF _ age dens)) -> VU.zip (VU.take 3 age) (VU.take 3 dens)
--   in simplifyResult calResult
-- :}
-- [(-1413,1.0703295e-5),(-1412,1.4653518e-5),(-1411,1.799142e-5)]
calibrateDateBchron :: CalibrationDistribution -> Bool -> Bool -> CalCurveBP -> UncalC14 -> Either CurrycarbonException CalPDF
calibrateDateBchron distr allowOutside interpolate calCurve uncalC14@(UncalC14 name age ageSd) =
    if not allowOutside && isOutsideRangeOfCalCurve calCurve uncalC14
    then Left $ CurrycarbonCalibrationRangeException $ _uncalC14Id uncalC14
    else 
        let rawCalCurveSegment = getRelevantCalCurveSegment uncalC14 calCurve
            CalCurveBCAD cals mus tau1s = prepareCalCurveSegment interpolate rawCalCurveSegment
            ageFloat = -(fromIntegral age)+1950
            ageSd2 = ageSd*ageSd
            ageSd2Float = fromIntegral ageSd2
            musFloat = VU.map fromIntegral mus
            tau1sFloat = VU.map fromIntegral tau1s
            dens = case distr of
                NormalDist -> 
                    VU.zipWith (\mu tau1 -> dnorm 0 1 ((ageFloat - mu) / sqrt (ageSd2Float + tau1 * tau1))) musFloat tau1sFloat
                StudentTDist degreesOfFreedom -> 
                    VU.zipWith (\mu tau1 -> dt degreesOfFreedom ((ageFloat - mu) / sqrt (ageSd2Float + tau1 * tau1))) musFloat tau1sFloat
        in Right $ trimLowDensityEdgesCalPDF $ normalizeCalPDF $ CalPDF name cals dens