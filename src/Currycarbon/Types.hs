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

newtype CalCurve = CalCurve [(Int, Int, Int)] -- BP, calBP, sigma
    deriving Show

-- getter functions

getBPsCal :: CalPDF -> [Int]
getBPsCal (CalPDF obs) = map fst obs

getProbsCal :: CalPDF -> [Double]
getProbsCal (CalPDF obs) = map snd obs

getBPsUncal :: UncalPDF -> [Int]
getBPsUncal (UncalPDF obs) = map fst obs

getProbsUncal :: UncalPDF -> [Double]
getProbsUncal (UncalPDF obs) = map snd obs

getBPs :: CalCurve -> [Int]
getBPs (CalCurve obs) = map (\(x,y,z) -> x) obs

getCals :: CalCurve -> [Int]
getCals (CalCurve obs) = map (\(x,y,z) -> y) obs

getCalSigmas :: CalCurve -> [Int]
getCalSigmas (CalCurve obs) = map (\(x,y,z) -> z) obs