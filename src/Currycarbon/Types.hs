module Currycarbon.Types where

data UncalC14 = UncalC14 Double Double

data UncalPDF = UncalPDF 
                [Double] -- BP
                [Double] -- probability
    deriving Show

data CalPDF = CalPDF 
                [Double] -- calBP
                [Double] -- probability
    deriving Show

newtype CalCurveMatrix = CalCurveMatrix [[Double]]
    deriving Show

data CalCurve = CalCurve 
                [Double] -- BP
                [Double] -- calBP
    deriving Show