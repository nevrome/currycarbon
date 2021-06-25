module Currycarbon.Types where

data UncalC14 = UncalC14 
                String -- name
                Int -- mean
                Int -- std
    deriving Show

data UncalPDF = UncalPDF String [(Int, Double)] -- Name, BP, probability
    deriving Show

data CalPDF = CalPDF String [(Int, Double)] -- Name, calBP, probability
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
getBPsCal (CalPDF _ obs) = map fst obs

getProbsCal :: CalPDF -> [Double]
getProbsCal (CalPDF _ obs) = map snd obs

getNameCal :: CalPDF -> String 
getNameCal (CalPDF name _) = name

getBPsUncal :: UncalPDF -> [Int]
getBPsUncal (UncalPDF _ obs) = map fst obs

getProbsUncal :: UncalPDF -> [Double]
getProbsUncal (UncalPDF _ obs) = map snd obs

getNameUncal :: UncalPDF -> String 
getNameUncal (UncalPDF name _) = name

getBPs :: CalCurve -> [Int]
getBPs (CalCurve obs) = map (\(x,y,z) -> x) obs

getCals :: CalCurve -> [Int]
getCals (CalCurve obs) = map (\(x,y,z) -> y) obs

getCalSigmas :: CalCurve -> [Int]
getCalSigmas (CalCurve obs) = map (\(x,y,z) -> z) obs