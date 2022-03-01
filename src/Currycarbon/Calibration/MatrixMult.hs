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

-- | Intercept calibration implemented with matrix multiplication (see 'MatrixMultiplication')
calibrateDateMatrixMult :: Bool -> Bool -> CalCurveBP -> UncalC14 -> Either CurrycarbonException CalPDF
calibrateDateMatrixMult allowOutside interpolate calCurve uncalC14 =
    if not allowOutside && isOutsideRangeOfCalCurve calCurve uncalC14
    then Left $ CurrycarbonCalibrationRangeException $ _uncalC14Id uncalC14
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
    let curveUnCalBCADsFloat = VU.map fromIntegral uncals
        sigmasFloat = VU.map fromIntegral sigmas
        uncalBCADs = vectorBPToBCAD uncals'
        uncalBCADsFloat = VU.map fromIntegral uncalBCADs
        matrix = buildMatrix curveUnCalBCADsFloat sigmasFloat uncalBCADsFloat
    in CalCurveMatrix uncalBCADs cals matrix
    where
        buildMatrix :: VU.Vector Float -> VU.Vector Float -> VU.Vector Float -> V.Vector (VU.Vector Float)
        buildMatrix curveuncal_ sigmas_ uncal_ =
          V.map (\x -> VU.map (fillCell x) uncal_) $ 
            V.zip (convert curveuncal_) (convert sigmas_)
        fillCell :: (Float, Float) -> Float -> Float
        fillCell (mean, sigma) matrixPosBP = 
            if abs (mean - matrixPosBP) < 6*sigma
            then dnorm mean sigma matrixPosBP
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
        vectorMatrixMultSum vec mat =
          convert $ V.map (\x -> VU.sum $ VU.zipWith (*) x vec) mat