module Currycarbon.Types where

data UncalC14 = UncalC14 
                Int -- mean
                Int -- std

data UncalPDF = UncalPDF 
                [Int] -- BP
                [Double] -- probability
    deriving Show

data CalPDF = CalPDF 
                [Int] -- calBP
                [Double] -- probability
    deriving Show

data CalCurveMatrix = CalCurveMatrix 
                [Int] -- BP
                [Int] -- calBP
                [[Double]] -- matrix
    deriving Show

newtype CalCurve = CalCurve [(Int, Int)] -- BP, calBP
    deriving Show