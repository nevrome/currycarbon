{-# LANGUAGE Strict #-}

module Currycarbon.Calibration.MatrixMult
    (   calibrateDateMatrixMult
      , makeCalCurveMatrix
      , uncalToPDF
    ) where

import           Currycarbon.Calibration.Utils
import           Currycarbon.Parsers
import           Currycarbon.Types
import           Currycarbon.Utils

import qualified Data.Vector                   as V
import           Data.Vector.Generic           (convert)
import qualified Data.Vector.Unboxed           as VU

-- | Intercept calibration implemented with matrix multiplication (see 'MatrixMultiplication')
calibrateDateMatrixMult :: Bool -> Bool -> CalCurveBP -> UncalC14 -> Either CurrycarbonException CalPDF
calibrateDateMatrixMult allowOutside interpolate calCurve uncalC14 =
    if not allowOutside && isOutsideRangeOfCalCurve calCurve uncalC14
    then Left $ CurrycarbonCalibrationRangeException $ renderUncalC14 uncalC14
    else
        let rawCalCurveSegment = getRelevantCalCurveSegment uncalC14 calCurve
            calCurveSegment = prepareCalCurveSegment interpolate rawCalCurveSegment
            uncalPDF = uncalToPDF uncalC14
            calCurveMatrix = makeCalCurveMatrix uncalPDF calCurveSegment
            calPDF = projectUncalOverCalCurve uncalPDF calCurveMatrix
        in Right $ trimLowDensityEdgesCalPDF $ normalizeCalPDF calPDF

-- | Construct a matrix representation of a calibration curve for a given date
makeCalCurveMatrix :: UncalPDF -> CalCurveBCAD -> CalCurveMatrix
makeCalCurveMatrix (UncalPDF _ uncals' _) (CalCurveBCAD cals uncals sigmas) =
    let curveUnCalBCADsDouble = VU.map fromIntegral uncals
        sigmasDouble = VU.map fromIntegral sigmas
        uncalBCADs = vectorBPToBCAD uncals'
        uncalBCADsDouble = VU.map fromIntegral uncalBCADs
        matrix = buildMatrix curveUnCalBCADsDouble sigmasDouble uncalBCADsDouble
    in CalCurveMatrix uncalBCADs cals matrix
    where
        buildMatrix :: VU.Vector Double -> VU.Vector Double -> VU.Vector Double -> V.Vector (VU.Vector Double)
        buildMatrix curveuncal_ sigmas_ uncal_ =
          V.map (\x -> VU.map (fillCell x) uncal_) $
            V.zip (convert curveuncal_) (convert sigmas_)
        fillCell :: (Double, Double) -> Double -> Double
        fillCell (mean, sigma) matrixPosBP =
            if abs (mean - matrixPosBP) < 6*sigma
            then dnorm mean sigma matrixPosBP
            else 0

-- | Transform an uncalibrated date to an uncalibrated
-- probability density table
uncalToPDF :: UncalC14 -> UncalPDF
uncalToPDF (UncalC14 name mean std) =
    let meanDouble = fromIntegral mean
        stdDouble = fromIntegral std
        years = VU.reverse $ VU.fromList [(mean-5*std) .. (mean+5*std)]
        yearsDouble = VU.map fromIntegral years
        probabilities = VU.map (dnorm meanDouble stdDouble) yearsDouble
    in UncalPDF name years probabilities

projectUncalOverCalCurve :: UncalPDF -> CalCurveMatrix -> CalPDF
projectUncalOverCalCurve (UncalPDF name _ dens) (CalCurveMatrix _ cals matrix) =
    CalPDF name cals $ vectorMatrixMultSum dens matrix
    where
        vectorMatrixMultSum :: VU.Vector Double -> V.Vector (VU.Vector Double) -> VU.Vector Double
        vectorMatrixMultSum vec mat =
          convert $ V.map (\x -> VU.sum $ VU.zipWith (*) x vec) mat
