{-# LANGUAGE Strict #-}

module Currycarbon.Calibration.MatrixMult
    (   calibrateDateMatrixMult
      , makeCalCurveMatrix
      , uncalToPDF
    ) where

import Currycarbon.Calibration.Utils
import Currycarbon.Types
import Currycarbon.Utils

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import Data.Vector.Generic (convert)

calibrateDateMatrixMult :: Bool -> Bool -> CalCurve -> UncalC14 -> Either CurrycarbonException CalPDF
calibrateDateMatrixMult allowOutside interpolate calCurve uncalC14 =
    if not allowOutside && isOutsideRangeOfCalCurve calCurve uncalC14
    then Left $ CurrycarbonCalibrationRangeException $ _uncalC14Id uncalC14
    else
        let rawCalCurveSegment = getRelevantCalCurveSegment uncalC14 calCurve
            calCurveSegment = prepareCalCurveSegment interpolate True rawCalCurveSegment
            uncalPDF = uncalToPDF uncalC14
            calCurveMatrix = makeCalCurveMatrix uncalPDF calCurveSegment
            calPDF = projectUncalOverCalCurve uncalPDF calCurveMatrix
        in Right $ trimLowDensityEdgesCalPDF $ normalizeCalPDF calPDF

-- | Construct a matrix representation of a calibration curve for a given date
makeCalCurveMatrix :: UncalPDF -> CalCurve -> CalCurveMatrix
makeCalCurveMatrix (UncalPDF _ bps' _) (CalCurve cals bps sigmas) =
    let bpsFloat = VU.map fromIntegral bps
        sigmasFloat = VU.map fromIntegral sigmas
        uncalbps = VU.map (\x -> -x+1950) bps'
        uncalbpsFloat = VU.map fromIntegral uncalbps
    in CalCurveMatrix uncalbps cals $ buildMatrix bpsFloat sigmasFloat uncalbpsFloat
    where
        buildMatrix :: VU.Vector Float -> VU.Vector Float -> VU.Vector Float -> V.Vector (VU.Vector Float)
        buildMatrix bps_ sigmas_ uncalbps_ = V.map (\x -> VU.map (fillCell x) uncalbps_) $ V.zip (convert bps_) (convert sigmas_)
        fillCell :: (Float, Float) -> Float -> Float
        fillCell (bp, sigma) matrixPosBP = 
            if abs (bp - matrixPosBP) < 6*sigma
            then dnorm bp sigma matrixPosBP
            else 0

-- | Transform an uncalibrated date to an uncalibrated 
-- probability density table
uncalToPDF :: UncalC14 -> UncalPDF
uncalToPDF (UncalC14 name mean std) =
    let meanFloat = fromIntegral mean
        stdFloat = fromIntegral std
        years = VU.reverse $ VU.fromList [(mean-5*std) .. (mean+5*std)]
        yearsFloat = VU.map fromIntegral years
        probabilities = VU.map (dnorm meanFloat stdFloat) yearsFloat
    in UncalPDF name years probabilities

projectUncalOverCalCurve :: UncalPDF -> CalCurveMatrix -> CalPDF
projectUncalOverCalCurve (UncalPDF name _ dens) (CalCurveMatrix _ cals matrix) =
    CalPDF name cals $ vectorMatrixMultSum dens matrix
    where
        vectorMatrixMultSum :: VU.Vector Float -> V.Vector (VU.Vector Float) -> VU.Vector Float
        vectorMatrixMultSum vec mat = convert $ V.map (\x -> VU.sum $ VU.zipWith (*) x vec) mat