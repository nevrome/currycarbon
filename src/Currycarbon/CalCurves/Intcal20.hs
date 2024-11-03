{-# LANGUAGE TemplateHaskell #-}

module Currycarbon.CalCurves.Intcal20 where

import           Currycarbon.Parsers (readCalCurve)
import           Currycarbon.Types   (CalCurveBP)
import qualified Data.FileEmbed      as FE

-- | The intcal20 calibration curve
-- (Reimer et al. 2020, doi: [10.1017/RDC.2020.41](https://doi.org/10.1017/RDC.2020.41))
intcal20 :: CalCurveBP
intcal20 = readCalCurve intcal20String
intcal20String :: String
intcal20String = $(FE.embedStringFile "data/intcal20.14c")
