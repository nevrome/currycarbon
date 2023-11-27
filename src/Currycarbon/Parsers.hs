{-# LANGUAGE OverloadedStrings #-}

module Currycarbon.Parsers where

import           Currycarbon.ParserHelpers
import           Currycarbon.Types
import           Currycarbon.Utils

import           Control.Exception         (throwIO)
import           Data.List                 (intercalate, transpose)
import qualified Data.Vector               as V
import qualified Data.Vector.Unboxed       as VU
import qualified Text.Parsec               as P
import qualified Text.Parsec.String        as P

-- * Parsing, rendering and writing functions
--
-- $importExport
--
-- This module contains a number of functions to manage data input and
-- output plumbing for different datatypes

-- read the calibration method

readCalibrationMethod :: String -> Either String CalibrationMethod
readCalibrationMethod s =
    case P.runParser parseCalibrationMethod () "" s of
        Left err -> Left $ showParsecErr err
        Right x  -> Right x

parseCalibrationMethod :: P.Parser CalibrationMethod
parseCalibrationMethod = do
    P.try bchron P.<|> matrixMultiplication
    where
        bchron = do
            _ <- P.string "Bchron,"
            P.try studentT P.<|> normal
        studentT = do
            _ <- P.string "StudentT,"
            dof <- parsePositiveDouble
            return (Bchron $ StudentTDist dof)
        normal = do
            _ <- P.string "Normal"
            return (Bchron NormalDist)
        matrixMultiplication = do
            _ <- P.string "MatrixMult"
            return MatrixMultiplication

-- pretty printing

-- | Combine 'CalExpr', 'CalPDF' and 'CalC14' to render pretty command line output
-- like this:
--
-- @
-- DATE: (5000±30BP + 5100±100BP)
-- Calibrated: 4150BC \>\> 3941BC \> 3814BC \< 3660BC \<\< 3651BC
-- 1-sigma: 3941-3864BC, 3810-3707BC, 3667-3660BC
-- 2-sigma: 4150-4148BC, 4048-3651BC
--                                           ▁
--                                           ▒▁ ▁▁
--                                   ▁▁▁    ▁▒▒▁▒▒
--                                 ▁▁▒▒▒    ▒▒▒▒▒▒
--                               ▁▁▒▒▒▒▒▁▁▁▁▒▒▒▒▒▒▁ ▁
--                           ▁▁▁▁▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▁▒▁
--         ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▁▁▁▁▁▁▁▁
--  -4330 ┄──┬─────┬─────┬─────┬──────┬─────┬─────┬─────┬─────┄ -3530
--                    \>            \>       \^         \<
--                                 ──────  ──────── ──
--                    ─     ──────────────────────────
-- @
--
renderCalDatePretty ::
       Bool -- ^ Should the CLI plot be restricted to (boring) ASCII symbols?
    -> (NamedCalExpr, CalPDF, CalC14)
    -> String
renderCalDatePretty ascii (calExpr, calPDF, calC14) =
    "DATE: " ++ intercalate "\n" [
          renderNamedCalExpr calExpr
        , renderCalC14 calC14
        , renderCLIPlotCalPDF ascii 6 50 calPDF calC14
        ]

-- write and read calibration expressions

renderNamedCalExpr :: NamedCalExpr -> String
renderNamedCalExpr (NamedCalExpr exprID calExpr) = renderExprID exprID ++ " " ++ renderCalExpr calExpr

renderExprID :: String -> String
renderExprID s = "[" ++ s ++ "]"

renderCalExpr :: CalExpr -> String
renderCalExpr (UnCalDate a)               = renderUncalC14 a
renderCalExpr (WindowBP a)                = renderTimeWindowBP a
renderCalExpr (WindowBCAD a)              = renderTimeWindowBCAD a
renderCalExpr (CalDate (CalPDF name _ _)) = name
renderCalExpr (SumCal a b)                = "(" ++ renderCalExpr a ++ " + " ++ renderCalExpr b ++ ")"
renderCalExpr (ProductCal a b)            = "(" ++ renderCalExpr a ++ " * " ++ renderCalExpr b ++ ")"

renderTimeWindowBP :: TimeWindowBP -> String
renderTimeWindowBP (TimeWindowBP name start stop) =
    name ++ ":" ++ renderYearBP start ++ "-" ++ renderYearBP stop

renderTimeWindowBCAD :: TimeWindowBCAD -> String
renderTimeWindowBCAD (TimeWindowBCAD name start stop) =
    name ++ ":" ++ renderYearBCAD start ++ "-" ++ renderYearBCAD stop

parseTimeWindowBP :: P.Parser TimeWindowBP
parseTimeWindowBP = parseRecordType "rangeBP" $ P.try long P.<|> short
    where
        long = do
            name  <- parseArgument "name" parseAnyString
            start <- parseArgument "start" parseWord
            stop  <- parseArgument "stop" parseWord
            construct name start stop
        short = do
            start <- parseArgument "start" parseWord
            stop  <- parseArgument "stop" parseWord
            construct "" start stop
        construct name start stop = do
            if start >= stop
            then return (TimeWindowBP name start stop)
            else fail "the BP stop date can not be larger than the start date"

parseTimeWindowBCAD :: P.Parser TimeWindowBCAD
parseTimeWindowBCAD = parseRecordType "rangeBCAD" $ P.try long P.<|> short
    where
        long = do
            name  <- parseArgument "name" parseAnyString
            start <- parseArgument "start" parseInt
            stop  <- parseArgument "stop" parseInt
            construct name start stop
        short = do
            start <- parseArgument "start" parseInt
            stop  <- parseArgument "stop" parseInt
            construct "" start stop
        construct name start stop = do
            if start <= stop
            then return (TimeWindowBCAD name start stop)
            else fail "the BC/AD stop date can not be smaller than the start date"

-- https://gist.github.com/abhin4v/017a36477204a1d57745
addFun :: P.Parser CalExpr
addFun = parseRecordType "sum" $ do
    a <- parseArgument "a" term
    b <- parseArgument "b" expr
    return $ SumCal a b

addOperator :: P.Parser CalExpr
addOperator = SumCal <$> term <*> (parseCharInSpace '+' *> expr)

mulFun :: P.Parser CalExpr
mulFun = parseRecordType "product" $ do
    a <- parseArgument "a" factor
    b <- parseArgument "b" term
    return $ ProductCal a b

mulOperator :: P.Parser CalExpr
mulOperator = ProductCal <$> factor <*> (parseCharInSpace '*' *> term)

parens :: P.Parser CalExpr
parens = P.between (parseCharInSpace '(') (parseCharInSpace ')') expr

factor :: P.Parser CalExpr
factor =      P.try parens
        P.<|> P.try addFun
        P.<|> P.try mulFun
        P.<|> P.try (WindowBP <$> parseTimeWindowBP)
        P.<|> P.try (WindowBCAD <$> parseTimeWindowBCAD)
        P.<|> (UnCalDate <$> parseUncalC14)

term :: P.Parser CalExpr
term = P.try mulOperator P.<|> factor

expr :: P.Parser CalExpr
expr = P.try addOperator P.<|> term -- <* P.eof

namedExpr :: P.Parser NamedCalExpr
namedExpr = P.try record P.<|> (NamedCalExpr "" <$> expr)
    where
        record = parseRecordType "calExpr" $ P.try long P.<|> short
        long = do
            name <- parseArgument "name" parseAnyString
            ex   <- parseArgument "expr" expr
            return (NamedCalExpr name ex)
        short = do
            ex   <- parseArgument "expr" expr
            return (NamedCalExpr "" ex)

readNamedCalExprs :: String -> Either String [NamedCalExpr]
readNamedCalExprs s =
    case P.runParser parseCalExprSepBySemicolon () "" s of
        Left err -> Left $ showParsecErr err
        Right x  -> Right x
        where
        parseCalExprSepBySemicolon :: P.Parser [NamedCalExpr]
        parseCalExprSepBySemicolon = P.sepBy namedExpr (P.char ';' <* P.spaces) <* P.eof

readOneNamedCalExpr :: String -> Either String NamedCalExpr
readOneNamedCalExpr s =
    case P.runParser namedExpr () "" s of
        Left err -> Left $ showParsecErr err
        Right x  -> Right x

readNamedCalExprsFromFile :: FilePath -> IO [NamedCalExpr]
readNamedCalExprsFromFile uncalFile = do
    s <- readFile uncalFile
    case P.runParser parseCalExprSepByNewline () "" s of
        Left err -> throwIO $ CurrycarbonCLIParsingException $ showParsecErr err
        Right x  -> return x
    where
        parseCalExprSepByNewline :: P.Parser [NamedCalExpr]
        parseCalExprSepByNewline = P.endBy namedExpr (P.newline <* P.spaces) <* P.eof

-- UncalC14
renderUncalC14WithoutName :: UncalC14 -> String
renderUncalC14WithoutName (UncalC14 _ bp sigma) = show bp ++ "±" ++ show sigma ++ "BP"

renderUncalC14 :: UncalC14 -> String
renderUncalC14 (UncalC14 name bp sigma) = name ++ ":" ++ show bp ++ "±" ++ show sigma ++ "BP"

-- | Read uncalibrated radiocarbon dates from a file. The file should feature one radiocarbon date
-- per line in the form "\<sample name\>,\<mean age BP\>,\<one sigma standard deviation\>", where
-- \<sample name\> is optional. A valid file could look like this:
--
-- @
-- Sample1,5000,30
-- 6000,50
-- Sample3,4000,25
-- @
--
readUncalC14FromFile :: FilePath -> IO [UncalC14]
readUncalC14FromFile uncalFile = do
    s <- readFile uncalFile
    case P.runParser uncalC14SepByNewline () "" s of
        Left err -> throwIO $ CurrycarbonCLIParsingException $ showParsecErr err
        Right x  -> return x
    where
        uncalC14SepByNewline :: P.Parser [UncalC14]
        uncalC14SepByNewline = P.endBy parseUncalC14 (P.newline <* P.spaces) <* P.eof

readUncalC14 :: String -> Either String [UncalC14]
readUncalC14 s =
    case P.runParser uncalC14SepBySemicolon () "" s of
        Left err -> Left $ showParsecErr err
        Right x  -> Right x
    where
        uncalC14SepBySemicolon :: P.Parser [UncalC14]
        uncalC14SepBySemicolon = P.sepBy parseUncalC14 (P.char ';' <* P.spaces) <* P.eof

parseUncalC14 :: P.Parser UncalC14
parseUncalC14 = P.try record P.<|> P.try long P.<|> short
    where
        record = parseRecordType "uncalC14" $ P.try long P.<|> short
        long = do
            name  <- parseArgument "name" parseAnyString
            age   <- parseArgument "age" parseWord
            sigma <- parseArgument "sigma" parseWord
            return (UncalC14 name age sigma)
        short = do
            age   <- parseArgument "age" parseWord
            sigma <- parseArgument "sigma" parseWord
            return (UncalC14 "" age sigma)

-- CalC14
-- | Write 'CalC14's to the file system. The output file is a long .csv file with the following structure:
--
-- @
-- sample,hdrSigma,hdrStartBCAD,hdrStopBCAD
-- Sample1,1,-3797,-3709
-- Sample1,1,-3894,-3880
-- Sample1,2,-3680,-3655
-- Sample1,2,-3810,-3700
-- Sample1,2,-3941,-3864
-- Sample2,1,-1142,-1130
-- Sample2,1,-1173,-1161
-- Sample2,1,-1293,-1194
-- Sample2,1,-1368,-1356
-- Sample2,2,-1061,-1059
-- Sample2,2,-1323,-1112
-- Sample2,2,-1393,-1334
-- @
--
writeCalC14s :: FilePath -> [CalC14] -> IO ()
writeCalC14s path calC14s = writeFile path $
    "sample,hdrSigma,hdrStartBCAD,hdrStopBCAD\n"
    ++ intercalate "\n" (map renderCalC14ForFile calC14s)

writeCalC14 :: FilePath -> CalC14 -> IO ()
writeCalC14 path calC14 = writeFile path $
    "sample,hdrSigma,hdrStartBCAD,hdrStopBCAD\n"
    ++ renderCalC14ForFile calC14

appendCalC14 :: FilePath -> CalC14 -> IO ()
appendCalC14 path calC14 =
    appendFile path $ "\n" ++ renderCalC14ForFile calC14

renderCalC14ForFile :: CalC14 -> String
renderCalC14ForFile (CalC14 name _ hdrs68 hdrs95) =
    intercalate "\n" $
        map renderRow $
        zip3 (repeat name) (repeat "1") (renderHDRsForFile hdrs68) ++
        zip3 (repeat name) (repeat "2") (renderHDRsForFile hdrs95)
    where
        renderRow :: (String, String, (String, String)) -> String
        renderRow (a, b, (c, d)) = intercalate "," [a,b,c,d]

renderCalC14s :: [CalC14] -> String
renderCalC14s xs =
    "Calibrated high density ranges (HDR):\n"
    ++ intercalate "\n" (map renderCalC14 xs)

renderCalC14 :: CalC14 -> String
renderCalC14 (CalC14 _ rangeSummary hdrs68 hdrs95) =
       "Calibrated: " ++ renderCalRangeSummary rangeSummary ++ "\n"
    ++ "1-sigma: " ++ renderHDRs hdrs68 ++ "\n"
    ++ "2-sigma: " ++ renderHDRs hdrs95

renderCalRangeSummary :: CalRangeSummary -> String
renderCalRangeSummary s =
       renderYearBCAD (_calRangeStartTwoSigma s) ++ " >> "
    ++ renderYearBCAD (_calRangeStartOneSigma s) ++ " > "
    ++ renderYearBCAD (_calRangeMedian s) ++ " < "
    ++ renderYearBCAD (_calRangeStopOneSigma s) ++ " << "
    ++ renderYearBCAD (_calRangeStopTwoSigma s)

-- BP
renderYearBP :: YearBP -> String
renderYearBP x =
    show x ++ "BP" -- ++ " (" ++ (renderYearBCAD $ bp2BCAD x) ++ ")"

-- BCAD
renderYearBCAD :: YearBCAD -> String
renderYearBCAD x
    | x < 0  = show (-x) ++ "BC"
    | x >= 0 = show x ++ "AD"
    | otherwise = error $ "This should never happen: " ++ show x

-- HDR
renderHDRsForFile :: [HDR] -> [(String, String)]
renderHDRsForFile = map renderHDRForFile

renderHDRForFile :: HDR -> (String, String)
renderHDRForFile (HDR start stop) = (show start, show stop)

renderHDRs :: [HDR] -> String
renderHDRs xs = intercalate ", " (map renderHDR xs)

renderHDR :: HDR -> String
renderHDR (HDR start stop)
    | start < 0 && stop <= 0  = show (-start) ++ "-" ++ show (-stop) ++ "BC"
    | start < 0 && stop > 0   = show (-start) ++ "BC-" ++ show stop ++ "AD"
    | start >= 0 && stop >= 0 = show start ++ "-" ++ show stop ++ "AD"
    | otherwise = error $ "This should never happen: " ++ show start ++ "-" ++ show stop

-- CalCurveMatrix
writeCalCurveMatrix :: FilePath -> CalCurveMatrix -> IO ()
writeCalCurveMatrix path calCurveMatrix =
    writeFile path $ renderCalCurveMatrix calCurveMatrix

renderCalCurveMatrix :: CalCurveMatrix -> String
renderCalCurveMatrix (CalCurveMatrix uncals cals curveDensities) =
    let header = "," ++ intercalate "," (map show $ VU.toList cals) ++ "\n"
        body = zipWith makeRow (VU.toList uncals) (transpose $ V.toList (V.map VU.toList curveDensities))
    in header ++ intercalate "\n" body
    where
      makeRow uncal dens = show uncal ++ "," ++ intercalate "," (map show dens)

-- CalPDF
-- | Write 'CalPDF's to the file system. The output file is a long .csv file with the following structure:
--
-- @
-- sample,calBCAD,density
-- ...
-- Sample1,-1391,2.8917924e-4
-- Sample1,-1390,3.3285577e-4
-- Sample1,-1389,3.5674628e-4
-- Sample1,-1388,3.750703e-4
-- ...
-- Sample2,-3678,1.8128564e-3
-- Sample2,-3677,1.9512239e-3
-- Sample2,-3676,2.0227064e-3
-- Sample2,-3675,2.095691e-3
-- ...
-- @
--
writeCalPDFs :: FilePath -> [CalPDF] -> IO ()
writeCalPDFs path calPDFs =
    writeFile path $
        "sample,calBCAD,density\n"
        ++ renderCalPDFs calPDFs

writeCalPDF :: FilePath -> CalPDF -> IO ()
writeCalPDF path calPDF =
    writeFile path $
        "sample,calBCAD,density\n"
        ++ renderCalPDF calPDF

appendCalPDF :: FilePath -> CalPDF -> IO ()
appendCalPDF path calPDF =
    appendFile path $ renderCalPDF calPDF

renderCalPDFs :: [CalPDF] -> String
renderCalPDFs = concatMap renderCalPDF

renderCalPDF :: CalPDF -> String
renderCalPDF (CalPDF name cals dens) =
    concatMap makeRow $ VU.toList $ VU.zip cals dens
    where
      makeRow (x,y) = name ++ "," ++ show x ++ "," ++ show y ++ "\n"

-- cli plot
data PlotSymbol = HistFill | HistTop | AxisEnd | AxisLine | AxisTick | HDRLine

renderCLIPlotCalPDF :: Bool -> Int -> Int -> CalPDF -> CalC14 -> String
renderCLIPlotCalPDF ascii rows cols (CalPDF _ cals dens) c14 =
     let startYear = VU.head cals
         stopYear = VU.last cals
         yearsPerCol = case quot (VU.length cals) cols of
            0 -> 1 -- relevant for very short PDFs
            1 -> 2
            q -> q
        -- last bin will often be shorter, which renders the whole plot slightly incorrect for the last column
         meanDensPerCol = calculateMeanDens yearsPerCol dens
         effectiveCols = length meanDensPerCol
         plotRows = map (replicate 8 ' ' ++) $ map (\x -> map (getHistSymbol x) meanDensPerCol) $ reverse [0..rows]
         xAxis = constructXAxis startYear stopYear effectiveCols yearsPerCol
     in intercalate "\n" plotRows ++ "\n" ++ xAxis
     where
        calculateMeanDens :: Int -> VU.Vector Float -> [Int]
        calculateMeanDens yearsPerCol dens_ =
            let scaling = fromIntegral rows
                meanDens = map (\x -> sum x / fromIntegral (length x)) $ splitEvery yearsPerCol $ VU.toList dens_
                maxDens = maximum meanDens
            in map (\x -> round $ (x / maxDens) * scaling) meanDens
        splitEvery :: Int -> [a] -> [[a]] -- https://stackoverflow.com/a/8681226/3216883
        splitEvery _ [] = []
        splitEvery n list = first : splitEvery n rest
            where (first,rest) = splitAt n list
        padString :: Int -> String -> String
        padString l x = replicate (l - length x) ' ' ++ x
        getSymbol :: Bool -> PlotSymbol -> Char
        getSymbol True HistFill  = '*'
        getSymbol False HistFill = '▒'
        getSymbol True HistTop   = '_'
        getSymbol False HistTop  = '▁'
        getSymbol True AxisEnd   = '+'
        getSymbol False AxisEnd  = '┄'
        getSymbol True AxisLine  = '-'
        getSymbol False AxisLine = '─'
        getSymbol True AxisTick  = '|'
        getSymbol False AxisTick = '┬'
        getSymbol True HDRLine   = '-'
        getSymbol False HDRLine  = '─'
        getHistSymbol :: Int -> Int -> Char
        getHistSymbol x y
            | x == y = getSymbol ascii HistTop
            | x < y  = getSymbol ascii HistFill
            | otherwise = ' '
        constructXAxis :: Int -> Int -> Int -> Int -> String
        constructXAxis startYear stopYear effCols yearsPerCol =
            let startS = padString 6 (show $ roundTo10 startYear)
                stopS = show (roundTo10 stopYear)
                tickFreq = if abs (startYear - stopYear) < 1500 then 100 else 1000
                colStartYears = map (\a -> startYear + yearsPerCol * a) [0..(effCols - 1)]
                colStopYears  = map (\b -> startYear + yearsPerCol * b - 1) [1..effCols]
                axis        = zipWith (getAxisSymbol tickFreq)                   colStartYears colStopYears
                simpleRange = zipWith (getRangeSymbol (_calC14RangeSummary c14)) colStartYears colStopYears
                hdrOne      = zipWith (getHDRSymbol (_calC14HDROneSigma c14))    colStartYears colStopYears
                hdrTwo      = zipWith (getHDRSymbol (_calC14HDRTwoSigma c14))    colStartYears colStopYears
            in  startS ++ (" " ++ [getSymbol ascii AxisEnd]) ++ axis ++ ([getSymbol ascii AxisEnd] ++ " ") ++ stopS ++ "\n" ++
                replicate 8 ' ' ++ simpleRange ++ "\n" ++
                replicate 8 ' ' ++ hdrOne ++ "\n" ++
                replicate 8 ' ' ++ hdrTwo
            where
                roundTo10 :: Int -> Int
                roundTo10 x =
                    let (dec,rest) = quotRem (abs x) 10
                        roundedDec = if rest >= 5 then dec + 1 else dec
                    in roundedDec * 10 * signum x
                getAxisSymbol :: Int -> Int -> Int -> Char
                getAxisSymbol tickFreq colStartYear colStopYear
                    | any (\x -> rem x tickFreq == 0) [colStartYear..colStopYear] = getSymbol ascii AxisTick
                    | otherwise = getSymbol ascii AxisLine
                getRangeSymbol :: CalRangeSummary -> Int -> Int -> Char
                getRangeSymbol range colStartYear colStopYear
                    | colStartYear <= _calRangeMedian range        && colStopYear >= _calRangeMedian range        = '^'
                    | colStartYear <= _calRangeStartOneSigma range && colStopYear >= _calRangeStartOneSigma range = '>'
                    | colStartYear <= _calRangeStopOneSigma range  && colStopYear >= _calRangeStopOneSigma range  = '<'
                    | colStartYear <= _calRangeStartTwoSigma range && colStopYear >= _calRangeStartTwoSigma range = '>'
                    | colStartYear <= _calRangeStopTwoSigma range  && colStopYear >= _calRangeStopTwoSigma range  = '<'
                    | otherwise = ' '
                getHDRSymbol :: [HDR] -> Int -> Int -> Char
                getHDRSymbol hdr colStartYear colStopYear
                    | any (doesOverlap colStartYear colStopYear) hdr = getSymbol ascii HDRLine
                    | otherwise = ' '
                    where
                        doesOverlap :: Int -> Int -> HDR -> Bool
                        doesOverlap a b h =
                            let ha = _hdrstart h; hb = _hdrstop h
                            in (a >= ha && a <= hb) || (b >= ha && b <= hb) || (a <= ha && b >= hb)

-- CalCurve
writeCalCurve :: FilePath -> CalCurveBCAD -> IO ()
writeCalCurve path calCurve =
    writeFile path $ renderCalCurve calCurve

renderCalCurve :: CalCurveBCAD -> String
renderCalCurve (CalCurveBCAD cals uncals sigmas) =
    let header = "calBCAD,uncalBCAD,Sigma\n"
        body = map makeRow $ VU.toList $ VU.zip3 cals uncals sigmas
    in header ++ intercalate "\n" body
    where
      makeRow (x,y,z) = show x ++ "," ++ show y ++ "," ++ show z

-- | Read a calibration curve file. The file must adhere to the current version of the
-- .c14 file format (e.g. [here](http://intcal.org/curves/intcal20.14c)). Look
-- [here](http://intcal.org/blurb.html) for other calibration curves
readCalCurveFromFile :: FilePath -> IO CalCurveBP
readCalCurveFromFile calCurveFile = do
    calCurve <- readFile calCurveFile
    return $ readCalCurve calCurve

readCalCurve :: String -> CalCurveBP
readCalCurve calCurveString = do
    case P.runParser parseCalCurve () "" calCurveString of
        Left p  -> error $ "This should never happen." ++ show p
        Right x -> CalCurveBP
            (VU.fromList $ map (\(a,_,_) -> a) x)
            (VU.fromList $ map (\(_,b,_) -> b) x)
            (VU.fromList $ map (\(_,_,c) -> c) x)

parseCalCurve :: P.Parser [(YearBP, YearBP, YearRange)]
parseCalCurve = do
    P.skipMany comments
    P.sepEndBy parseCalCurveLine (P.manyTill P.anyToken (P.try P.newline))

parseCalCurveLine :: P.Parser (YearBP, YearBP, YearRange)
parseCalCurveLine = do
  calBP <- parseWord
  _ <- P.oneOf ","
  bp <- parseWord
  _ <- P.oneOf ","
  sigma <- parseWord
  return (calBP, bp, sigma)

comments :: P.Parser String
comments = do
    _ <- P.string "#"
    _ <- P.manyTill P.anyChar P.newline
    return ""

-- RandomAgeSamples
-- | Write 'RandomAgeSamples's to the file system. The output file is a long .csv file with the following structure:
--
-- @
-- sample,calBCAD
-- ...
-- Sample1,-1221
-- Sample1,-1211
-- Sample1,-1230
-- Sample1,-1225
-- ...
-- Sample2,-3763
-- Sample2,-3788
-- Sample2,-3767
-- Sample2,-3774
-- ...
-- @
--
writeRandomAgeSamples :: FilePath -> [RandomAgeSample] -> IO ()
writeRandomAgeSamples path calPDFs =
    writeFile path $
        "sample,calBCAD,density\n"
        ++ renderRandomAgeSamples calPDFs

writeRandomAgeSample :: FilePath -> RandomAgeSample -> IO ()
writeRandomAgeSample path calPDF =
    writeFile path $
        "sample,calBCAD,density\n"
        ++ renderRandomAgeSample calPDF

appendRandomAgeSample :: FilePath -> RandomAgeSample -> IO ()
appendRandomAgeSample path calPDF =
    appendFile path $ renderRandomAgeSample calPDF

renderRandomAgeSamples :: [RandomAgeSample] -> String
renderRandomAgeSamples = concatMap renderRandomAgeSample

renderRandomAgeSample :: RandomAgeSample -> String
renderRandomAgeSample (RandomAgeSample name samples) =
    concatMap makeRow $ VU.toList samples
    where
      makeRow x = name ++ "," ++ show x ++ "\n"
