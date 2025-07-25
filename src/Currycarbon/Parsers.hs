{-# LANGUAGE OverloadedStrings #-}

module Currycarbon.Parsers where

import           Currycarbon.CalCurves         (intcal20)
import           Currycarbon.Calibration.Utils
import           Currycarbon.ParserHelpers
import           Currycarbon.Types
import           Currycarbon.Utils

import           Control.Exception             (throwIO)
import           Data.List                     (intercalate, transpose)
import qualified Data.Vector                   as V
import qualified Data.Vector.Unboxed           as VU
import qualified Text.Parsec                   as P
import qualified Text.Parsec.String            as P

-- * Parsing, rendering and writing functions
--
-- $importExport
--
-- This module contains a number of functions to manage data input and
-- output plumbing for different datatypes

-- read the calibration method

readCalibrationMethod :: String -> Either String CalibrationMethod
readCalibrationMethod s =
    case P.runParser parseCalibrationMethod () s s of
        Left err -> Left $ showParsecErrOneLine err
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
-- CalEXPR: [1] (S1:5000±30BP + S2:5100±100BP)
-- Calibrated: 4150BC >> 3941BC > 3814BC < 3660BC << 3651BC
-- 1-sigma: 3941-3864BC, 3810-3707BC, 3667-3660BC
-- 2-sigma: 4150-4147BC, 4048-3651BC
--
--
--                                              ▁
--                                       ▁     ▁▒▁ ▁▁
--                                      ▁▒     ▒▒▒▁▒▒
--                                    ▁▁▒▒▁   ▁▒▒▒▒▒▒
--                                 ▁▁▁▒▒▒▒▒▁▁▁▒▒▒▒▒▒▒▁  ▁
--                             ▁▁▁▁▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▁▁▒
--          ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▁▁▁▁▁▁▁▁▁
-- -4330 ┄─┬──────┬──────┬─────┬──────┬──────┬─────┬──────┬─────┄ -3530
--    BC              >             >        ^         <<         BC
--                                   ──────   ────────  ─
--                     ──     ────────────────────────────
-- @
--
renderCalDatePretty ::
       Bool -- ^ Should the CLI plot be restricted to (boring) ASCII symbols?
    -> (NamedCalExpr, CalPDF, CalC14)
    -> String
renderCalDatePretty ascii (calExpr, calPDF, calC14) =
    "CalEXPR: " ++ intercalate "\n" [
          renderNamedCalExpr calExpr
        , renderCalC14 calC14
        , ""
        , renderCLIPlotCalCurve ascii 8 50 calPDF calExpr
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
            name  <- parseArgument "id" parseAnyString
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
            name  <- parseArgument "id" parseAnyString
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
namedExpr = P.try nameBeforeColon P.<|> P.try record P.<|> onlyExpr
    where
        nameBeforeColon = do
            name <- parseAnyString
            _ <- P.char ':'
            _ <- P.spaces
            ex <- expr
            return (NamedCalExpr name ex)
        record = parseRecordType "calExpr" $ P.try long P.<|> short
        long = do
            name <- parseArgument "id" parseAnyString
            ex   <- parseArgument "expr" expr
            return (NamedCalExpr name ex)
        short = do
            ex   <- parseArgument "expr" expr
            return (NamedCalExpr "" ex)
        onlyExpr = NamedCalExpr "" <$> expr

readNamedCalExprs :: String -> Either String [NamedCalExpr]
readNamedCalExprs s =
    case P.runParser parseCalExprSepBySemicolon () s s of
        Left err -> Left $ showParsecErrOneLine err
        Right x  -> Right x
        where
        parseCalExprSepBySemicolon :: P.Parser [NamedCalExpr]
        parseCalExprSepBySemicolon = P.sepBy namedExpr (P.char ';' <* P.spaces) <* P.eof

readOneNamedCalExpr :: String -> Either String NamedCalExpr
readOneNamedCalExpr s =
    case P.runParser namedExpr () s s of
        Left err -> Left $ showParsecErrOneLine err
        Right x  -> Right x

readNamedCalExprsFromFile :: FilePath -> IO [NamedCalExpr]
readNamedCalExprsFromFile uncalFile = do
    ss <- lines <$> readFile uncalFile
    mapM readOneLine ss
    where
        readOneLine :: String -> IO NamedCalExpr
        readOneLine s = case readOneNamedCalExpr s of
            Left err -> throwIO $ CurrycarbonCLIParsingException $ err ++ "\nin \"" ++ s ++ "\""
            Right x  -> return x

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
    case P.runParser uncalC14SepByNewline () uncalFile s of
        Left err -> throwIO $ CurrycarbonCLIParsingException $ showParsecErr err
        Right x  -> return x
    where
        uncalC14SepByNewline :: P.Parser [UncalC14]
        uncalC14SepByNewline = P.endBy parseUncalC14 (P.newline <* P.spaces) <* P.eof

readUncalC14 :: String -> Either String [UncalC14]
readUncalC14 s =
    case P.runParser uncalC14SepBySemicolon () s s of
        Left err -> Left $ showParsecErrOneLine err
        Right x  -> Right x
    where
        uncalC14SepBySemicolon :: P.Parser [UncalC14]
        uncalC14SepBySemicolon = P.sepBy parseUncalC14 (P.char ';' <* P.spaces) <* P.eof

parseUncalC14 :: P.Parser UncalC14
parseUncalC14 = P.try record P.<|> P.try long P.<|> short
    where
        record = parseRecordType "uncalC14" $ P.try long P.<|> short
        long = do
            name  <- parseArgument "id" parseAnyString
            age   <- parseArgument "yearBP" parseWord
            sigma <- parseArgument "sigma" parseWord
            return (UncalC14 name age sigma)
        short = do
            age   <- parseArgument "yearBP" parseWord
            sigma <- parseArgument "sigma" parseWord
            return (UncalC14 "" age sigma)

-- CalC14
renderCalC14s :: [CalC14] -> String
renderCalC14s xs =
    "Calibrated high density ranges (HDR):\n"
    ++ intercalate "\n" (map renderCalC14 xs)

renderCalC14 :: CalC14 -> String
renderCalC14 (CalC14 _ rangeSummary hdrs68 hdrs95) =
       "Calibrated: " ++ renderCalRangeSummary rangeSummary ++ "\n"
    ++ "1-sigma: " ++ renderHDRs hdrs68 ++ "\n"
    ++ "2-sigma: " ++ renderHDRs hdrs95

-- CalC14 - CalRangeSummary
-- | Write 'CalRangeSummary's to the file system. The output file is a .tsv file with the following structure:
--
-- @
-- id	startTwoSigmaYearBCAD	startOneSigmaYearBCAD	medianYearBCAD	stopOneSigmaYearBCAD	stopTwoSigmaYearBCAD
-- Sample1	-3941	-3894	-3773	-3709	-3655
-- Sample3	-2572	-2566	-2527	-2472	-2467
-- @
--
writeCalC14CalRangeSummaries :: FilePath -> [CalC14] -> IO ()
writeCalC14CalRangeSummaries path calC14s = writeFile path $
    "id\tstartTwoSigmaYearBCAD\tstartOneSigmaYearBCAD\tmedianYearBCAD\tstopOneSigmaYearBCAD\tstopTwoSigmaYearBCAD\n"
    ++ intercalate "\n" (map renderCalC14CalRangeSummaryForFile calC14s)

writeCalC14CalRangeSummary :: FilePath -> CalC14 -> IO ()
writeCalC14CalRangeSummary path calC14 = writeFile path $
    "id\tstartTwoSigmaYearBCAD\tstartOneSigmaYearBCAD\tmedianYearBCAD\tstopOneSigmaYearBCAD\tstopTwoSigmaYearBCAD\n"
    ++ renderCalC14CalRangeSummaryForFile calC14

appendCalC14CalRangeSummary :: FilePath -> CalC14 -> IO ()
appendCalC14CalRangeSummary path calC14 =
    appendFile path $ "\n" ++ renderCalC14CalRangeSummaryForFile calC14

renderCalC14CalRangeSummaryForFile :: CalC14 -> String
renderCalC14CalRangeSummaryForFile (CalC14 name (CalRangeSummary start2 start1 median stop1 stop2) _ _) =
    intercalate "\t" $ name:map show [start2,start1,median,stop1,stop2]

-- CalRangeSummary
renderCalRangeSummary :: CalRangeSummary -> String
renderCalRangeSummary s =
       renderYearBCAD (_calRangeStartTwoSigma s) ++ " >> "
    ++ renderYearBCAD (_calRangeStartOneSigma s) ++ " > "
    ++ renderYearBCAD (_calRangeMedian s) ++ " < "
    ++ renderYearBCAD (_calRangeStopOneSigma s) ++ " << "
    ++ renderYearBCAD (_calRangeStopTwoSigma s)

-- CalC14 - HDR
-- | Write 'HDR's to the file system. The output file is a long .tsv file with the following structure:
--
-- @
-- id  hdrSigmaLevel  hdrStartYearBCAD  hdrStopYearBCAD
-- Sample1  1	-3797	-3709
-- Sample1  1	-3894	-3880
-- Sample1  2	-3680	-3655
-- Sample1  2	-3810	-3700
-- Sample1  2	-3941	-3864
-- Sample2  1	-1142	-1130
-- Sample2	1	-1173	-1161
-- Sample2	1	-1293	-1194
-- Sample2	1	-1368	-1356
-- Sample2	2	-1061	-1059
-- Sample2	2	-1323	-1112
-- Sample2	2	-1393	-1334
-- @
--
writeCalC14HDRs :: FilePath -> [CalC14] -> IO ()
writeCalC14HDRs path calC14s = writeFile path $
    "id\thdrSigmaLevel\thdrStartYearBCAD\thdrStopYearBCAD\n"
    ++ intercalate "\n" (map renderCalC14HDRForFile calC14s)

writeCalC14HDR :: FilePath -> CalC14 -> IO ()
writeCalC14HDR path calC14 = writeFile path $
    "id\thdrSigmaLevel\thdrStartYearBCAD\thdrStopYearBCAD\n"
    ++ renderCalC14HDRForFile calC14

appendCalC14HDR :: FilePath -> CalC14 -> IO ()
appendCalC14HDR path calC14 =
    appendFile path $ "\n" ++ renderCalC14HDRForFile calC14

renderCalC14HDRForFile :: CalC14 -> String
renderCalC14HDRForFile (CalC14 name _ hdrs68 hdrs95) =
    intercalate "\n" $
        map renderRow $
        zip3 (repeat name) (repeat "1") (renderHDRsForFile hdrs68) ++
        zip3 (repeat name) (repeat "2") (renderHDRsForFile hdrs95)
    where
        renderRow :: (String, String, (String, String)) -> String
        renderRow (a, b, (c, d)) = intercalate "\t" [a,b,c,d]

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

-- HDR for CLI output
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
    let header = "\t" ++ intercalate "\t" (map show $ VU.toList cals) ++ "\n"
        body = zipWith makeRow (VU.toList uncals) (transpose $ V.toList (V.map VU.toList curveDensities))
    in header ++ intercalate "\n" body
    where
      makeRow uncal dens = show uncal ++ "\t" ++ intercalate "\t" (map show dens)

-- CalPDF
-- | Write 'CalPDF's to the file system. The output file is a long .tsv file with the following structure:
--
-- @
-- id  yearBCAD  density
-- ...
-- Sample1  -1391   2.8917924e-4
-- Sample1  -1390   3.3285577e-4
-- Sample1  -1389   3.5674628e-4
-- Sample1  -1388   3.750703e-4
-- ...
-- Sample2  -3678   1.8128564e-3
-- Sample2  -3677   1.9512239e-3
-- Sample2  -3676   2.0227064e-3
-- Sample2  -3675   2.095691e-3
-- ...
-- @
--
writeCalPDFs :: FilePath -> [CalPDF] -> IO ()
writeCalPDFs path calPDFs =
    writeFile path $
        "id\tyearBCAD\tdensity\n"
        ++ renderCalPDFs calPDFs

writeCalPDF :: FilePath -> CalPDF -> IO ()
writeCalPDF path calPDF =
    writeFile path $
        "id\tyearBCAD\tdensity\n"
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
      makeRow (x,y) = name ++ "\t" ++ show x ++ "\t" ++ show y ++ "\n"

-- cli plot
data PlotSymbol =
    -- density histogram
      HistFill | HistTop | AxisEnd | AxisLine | AxisTick | HDRLine
    -- calcurve plot
    | CalCurve | BPLine | RibbonLine | YAxisLine | YAxisTick

getSymbol :: Bool -> PlotSymbol -> Char
-- density histogram
getSymbol True  HistFill   = '*'
getSymbol False HistFill   = '▒'
getSymbol True  HistTop    = '_'
getSymbol False HistTop    = '▁'
getSymbol True  AxisEnd    = '+'
getSymbol False AxisEnd    = '┄'
getSymbol True  AxisLine   = '-'
getSymbol False AxisLine   = '─'
getSymbol True  AxisTick   = '|'
getSymbol False AxisTick   = '┬'
getSymbol True  HDRLine    = '-'
getSymbol False HDRLine    = '─'
-- calcurve plot
getSymbol True CalCurve    = '|'
getSymbol False CalCurve   = '┆'
getSymbol True BPLine      = '-'
getSymbol False BPLine     = '┅'
getSymbol True RibbonLine  = '-'
getSymbol False RibbonLine = '┄'
getSymbol True YAxisLine   = '|'
getSymbol False YAxisLine  = '│'
getSymbol True YAxisTick   = '|'
getSymbol False YAxisTick  = '┤'

splitEvery :: Int -> [a] -> [[a]] -- https://stackoverflow.com/a/8681226/3216883
splitEvery _ [] = []
splitEvery n list = first : splitEvery n rest
    where (first,rest) = splitAt n list

avg :: [Double] -> Double
avg x = sum x / fromIntegral (length x)

padString :: Int -> String -> String
padString l x = replicate (l - length x) ' ' ++ x

roundTo10 :: Int -> Int
roundTo10 x =
    let (dec,rest) = quotRem (abs x) 10
        roundedDec = if rest >= 5 then dec + 1 else dec
    in roundedDec * 10 * signum x

renderCLIPlotCalCurve :: Bool -> Int -> Int -> CalPDF -> NamedCalExpr -> String
renderCLIPlotCalCurve
    ascii rows cols (CalPDF _ cals _)
    (NamedCalExpr _ (UnCalDate (UncalC14 _ yearBP sigma))) =
    let startYear = VU.head cals
        stopYear = VU.last cals
        -- prepare calcurve
        calcurvePrep = makeBCADCalCurve $ interpolateCalCurve intcal20
        calCurveSegment = punchOutCalCurveBCAD startYear stopYear calcurvePrep
        calCurveUncals = VU.map fromIntegral $ _calCurveBCADUnCals calCurveSegment
        calCurveUncalStart = bcad2BP $ round $ VU.head calCurveUncals
        calCurveUncalStop = bcad2BP $ round $ VU.last calCurveUncals
        yearsPerCol = case quot (VU.length calCurveUncals) cols of
            0 -> 1 -- relevant for very short PDFs
            1 -> 2
            q -> q
        meanUncalPerCol = map avg $ splitEvery yearsPerCol $ VU.toList calCurveUncals
        meanYearsPerCol = map rescale meanUncalPerCol
        -- rescaling setup for rendering to correct size
        minUncalYear = minimum meanUncalPerCol
        maxUncalYear = maximum meanUncalPerCol
        rescale = rescaleToRows minUncalYear maxUncalYear
        -- prepare static elements for uncal date
        uncalAgePlusSigma = rescale $ fromIntegral $ bp2BCAD (yearBP + sigma)
        uncalAge = rescale $ fromIntegral $ bp2BCAD yearBP
        uncalAgeMinusSigma = rescale $ fromIntegral $ bp2BCAD (yearBP - sigma)
        -- perform row-wise rendering
        renderYAxis = yAxis calCurveUncalStart calCurveUncalStop uncalAge
        renderRow = getLineSymbol uncalAgeMinusSigma uncalAge uncalAgePlusSigma
        plotRows = map (\r -> renderYAxis r ++ map (renderRow r) meanYearsPerCol) [0..rows]
        axisUnitLine = replicate 4 ' ' ++ "BP"
    in  intercalate "\n" $ axisUnitLine:plotRows
    where
        yAxis :: Word -> Word -> Int -> Int -> String
        yAxis ysta ysto a x
            | a == x = makeTick yearBP
            | x == 0 = makeTick ysta
            | x == 8 = makeTick ysto
            | otherwise = replicate 6 ' ' ++ " " ++ getSymbol ascii YAxisLine : " "
        rescaleToRows :: Double -> Double -> Double -> Int
        rescaleToRows minVal maxVal x =
            let range  = maxVal - minVal
                scaler = fromIntegral rows / range
            in (round . (*) scaler . subtract minVal) x
        getLineSymbol :: Int -> Int -> Int -> Int -> Int -> Char
        getLineSymbol ma a pa x y
            | x == y = getSymbol ascii CalCurve
            | a == x = getSymbol ascii BPLine
            | x == pa = getSymbol ascii RibbonLine
            | x == ma = getSymbol ascii RibbonLine
            | otherwise = ' '
        makeTick :: (Integral n) => n -> String
        makeTick n = padString 6 (show $ roundTo10 $ fromIntegral n) ++ " " ++ getSymbol ascii YAxisTick : " "
renderCLIPlotCalCurve _ _ _ _ _ = ""

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
         plotRows = map (replicate 9 ' ' ++) $ map (\x -> map (getHistSymbol x) meanDensPerCol) $ reverse [0..rows]
         xAxis = constructXAxis startYear stopYear effectiveCols yearsPerCol
     in intercalate "\n" plotRows ++ "\n" ++ xAxis
     where
        calculateMeanDens :: Int -> VU.Vector Double -> [Int]
        calculateMeanDens yearsPerCol dens_ =
            let scaling = fromIntegral rows
                meanDens = map (\x -> sum x / fromIntegral (length x)) $ splitEvery yearsPerCol $ VU.toList dens_
                maxDens = maximum meanDens
            in map (\x -> round $ (x / maxDens) * scaling) meanDens
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
                replicate 4 ' ' ++ getADBC startYear ++ "   " ++ simpleRange ++ " " ++ getADBC stopYear ++ "\n" ++
                replicate 9 ' ' ++ hdrOne ++ "\n" ++
                replicate 9 ' ' ++ hdrTwo
            where
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
                getADBC :: Int -> String
                getADBC y
                    | y < 0 = "BC"
                    | otherwise = "AD"

-- CalCurve
writeCalCurve :: FilePath -> CalCurveBCAD -> IO ()
writeCalCurve path calCurve =
    writeFile path $ renderCalCurve calCurve

renderCalCurve :: CalCurveBCAD -> String
renderCalCurve (CalCurveBCAD cals uncals sigmas) =
    let header = "calYearBCAD\tuncalYearBCAD\tsigma\n"
        body = map makeRow $ VU.toList $ VU.zip3 cals uncals sigmas
    in header ++ intercalate "\n" body
    where
      makeRow (x,y,z) = show x ++ "\t" ++ show y ++ "\t" ++ show z

-- RandomAgeSamples
-- | Write 'RandomAgeSamples's to the file system. The output file is a long .tsv file with the following structure:
--
-- @
-- id  yearBCAD
-- ...
-- Sample1  -1221
-- Sample1  -1211
-- Sample1  -1230
-- Sample1  -1225
-- ...
-- Sample2  -3763
-- Sample2  -3788
-- Sample2  -3767
-- Sample2  -3774
-- ...
-- @
--
writeRandomAgeSamples :: FilePath -> [RandomAgeSample] -> IO ()
writeRandomAgeSamples path calPDFs =
    writeFile path $
        "id\tyearBCAD\n"
        ++ renderRandomAgeSamples calPDFs

writeRandomAgeSample :: FilePath -> RandomAgeSample -> IO ()
writeRandomAgeSample path calPDF =
    writeFile path $
        "id\tyearBCAD\n"
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
      makeRow x = name ++ "\t" ++ show x ++ "\n"
