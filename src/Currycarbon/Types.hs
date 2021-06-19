module Currycarbon.Types where

import           Data.Matrix
import qualified Data.Vector as V

data UncalC14 = UncalC14 Double Double

newtype UncalPDF = UncalPDF (V.Vector (Double, Double)) 
    deriving Show

newtype CalPDF = CalPDF (V.Vector (Double, Double)) 
    deriving Show

newtype CalCurveSegment = CalCurveSegment (Matrix Double)