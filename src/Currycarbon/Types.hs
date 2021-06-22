module Currycarbon.Types where

data UncalC14 = UncalC14 
                Int -- mean
                Int -- std

newtype UncalPDF = UncalPDF [(Int, Double)] -- BP, probability
    deriving Show

newtype CalPDF = CalPDF [(Int, Double)] -- calBP, probability
    deriving Show

data CalCurveMatrix = CalCurveMatrix 
                [Int] -- BP
                [Int] -- calBP
                [[Double]] -- matrix
    deriving Show

newtype CalCurve = CalCurve [(Int, Int)] -- BP, calBP
    deriving Show