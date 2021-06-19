module Currycarbon.Types where

import qualified Data.Vector as V

data UncalC14 = UncalC14 Double Double

newtype UncalPDF = UncalPDF (V.Vector (Double, Double)) 
    deriving Show