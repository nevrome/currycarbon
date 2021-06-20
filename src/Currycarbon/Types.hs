module Currycarbon.Types where

data UncalC14 = UncalC14 Double Double

data UncalPDF = UncalPDF 
                [Double] -- years
                [Double] -- probability
    deriving Show

data CalPDF = CalPDF 
                [Double] -- years
                [Double] -- probability
    deriving Show

newtype CalCurveMatrix = CalCurveMatrix [[Double]]