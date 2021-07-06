module Currycarbon.Types where

data UncalC14 = UncalC14 
                String -- name
                Int -- mean
                Int -- std
    deriving Show

data UncalPDF = UncalPDF 
                String -- Name
                [(Int, Float)] -- BP, probability
    deriving Show

data CalPDF = CalPDF 
                String -- Name
                [(Int, Float)] -- calBP, probability
    deriving (Show)

data CalC14 = CalC14 
                String -- Name
                [HDR] -- 1sigma high density regions
                [HDR] -- 2sigma high density regions
    deriving Show

data HDR = HDR -- High density region
                Int -- Start age calBC
                Int -- Stop age calBC
    deriving Show

data CalCurveMatrix = CalCurveMatrix 
                [Int] -- BP
                [Int] -- calBP
                [[Float]] -- matrix
    deriving Show

newtype CalCurve = CalCurve [(Int, Int, Int)] -- BP, calBP, sigma
    deriving Show

-- getter functions

getBPsCal :: CalPDF -> [Int]
getBPsCal (CalPDF _ obs) = map fst obs

getProbsCal :: CalPDF -> [Float]
getProbsCal (CalPDF _ obs) = map snd obs

getNameCal :: CalPDF -> String 
getNameCal (CalPDF name _) = name

getBPsUncal :: UncalPDF -> [Int]
getBPsUncal (UncalPDF _ obs) = map fst obs

getProbsUncal :: UncalPDF -> [Float]
getProbsUncal (UncalPDF _ obs) = map snd obs

getNameUncal :: UncalPDF -> String 
getNameUncal (UncalPDF name _) = name

getBPs :: CalCurve -> [Int]
getBPs (CalCurve obs) = map (\(x,_,_) -> x) obs

getCals :: CalCurve -> [Int]
getCals (CalCurve obs) = map (\(_,y,_) -> y) obs

getCalSigmas :: CalCurve -> [Int]
getCalSigmas (CalCurve obs) = map (\(_,_,z) -> z) obs