{-# LANGUAGE OverloadedStrings #-}

module Currycarbon.TSV where

import Currycarbon.Types

import           Control.Applicative    (empty)
import qualified Data.ByteString.Char8  as B8
import qualified Data.ByteString.Lazy   as BL
import qualified Data.Csv               as Csv
import qualified Data.HashMap.Strict    as HM
import qualified Data.Vector            as V
import           Data.Char              (ord)

readTSV :: FilePath -> IO TSV
readTSV path = do
    bs <- BL.readFile path
    case Csv.decodeByNameWith (Csv.DecodeOptions $ fromIntegral (ord '\t')) bs of
      Left err -> fail err
      Right (header, rows) -> return (TSV path header rows)

tsv2NamedCalExprs :: TSV -> [NamedCalExpr]
tsv2NamedCalExprs (TSV _ _ rows) = V.toList $ V.map tsvRow2NamedCalExprs rows

tsvRow2NamedCalExprs :: TSVRow -> NamedCalExpr
tsvRow2NamedCalExprs (TSVRow i (Just (ListColumn lcs)) (Just (ListColumn bps)) (Just (ListColumn errs)) _ _ _ _) |
    (length lcs == length bps) && (length bps == length errs) =
    NamedCalExpr i $ foldCalExpr $ zip3 lcs bps errs
tsvRow2NamedCalExprs (TSVRow i _ _ _ (Just start) _ (Just stop) _) =
    NamedCalExpr i $  WindowBCAD (TimeWindowBCAD "" start stop)
tsvRow2NamedCalExprs (TSVRow _ _ _ _ _ _ _ _) = undefined

foldCalExpr :: [(String, Word, Word)] -> CalExpr
foldCalExpr xs = foldl1 SumCal $ map (\(lc,bp,err) -> UnCalDate $ UncalC14 lc bp err) xs

data TSV = TSV {
      _tsvFile :: FilePath
    , _tsvHeader :: Csv.Header
    , _tsvRows :: V.Vector TSVRow
    }

data TSVRow = TSVRow {
      _tsvRowID            :: String
    , _tsvRowC14Labnr      :: Maybe (ListColumn String)
    , _tsvRowC14UncalBP    :: Maybe (ListColumn Word)
    , _tsvRowC14UncalBPErr :: Maybe (ListColumn Word)
    , _tsvRowBCADStart     :: Maybe Int
    , _tsvRowBCADMedian    :: Maybe Int
    , _tsvRowBCADStop      :: Maybe Int
    , _otherColumns      :: Csv.NamedRecord
    }
    deriving Show

instance Csv.FromNamedRecord TSVRow where
    parseNamedRecord m = do
        i          <- filterLookup m "Date_ID"
        labnr      <- filterLookupOptional m "Date_C14_Labnr"
        uncalBP    <- filterLookupOptional m "Date_C14_Uncal_BP"
        uncalBPErr <- filterLookupOptional m "Date_C14_Uncal_BP_Err"
        start      <- filterLookupOptional m "Date_BC_AD_Start"
        median     <- filterLookupOptional m "Date_BC_AD_Median"
        stop       <- filterLookupOptional m "Date_BC_AD_Stop"
        pure $ TSVRow {
              _tsvRowID            = i
            , _tsvRowC14Labnr      = labnr
            , _tsvRowC14UncalBP    = uncalBP
            , _tsvRowC14UncalBPErr = uncalBPErr
            , _tsvRowBCADStart     = start
            , _tsvRowBCADMedian    = median
            , _tsvRowBCADStop      = stop
            , _otherColumns      = m
            }

filterLookup :: Csv.FromField a => Csv.NamedRecord -> B8.ByteString -> Csv.Parser a
filterLookup m name = maybe empty Csv.parseField $ cleanInput $ HM.lookup name m

filterLookupOptional :: Csv.FromField a => Csv.NamedRecord -> B8.ByteString -> Csv.Parser (Maybe a)
filterLookupOptional m name = maybe (pure Nothing) (\bs -> Just <$> Csv.parseField bs) $
                              cleanInput $ HM.lookup name m

cleanInput :: Maybe B8.ByteString -> Maybe B8.ByteString
cleanInput Nothing           = Nothing
cleanInput (Just rawInputBS) = transNA rawInputBS
    where
        transNA ".."  = Nothing
        transNA ""    = Nothing
        transNA "n/a" = Nothing
        transNA x     = Just x

newtype ListColumn a = ListColumn {getListColumn :: [a]}
    deriving (Eq, Ord, Show)
instance (Csv.FromField a) => Csv.FromField (ListColumn a) where
    parseField x = fmap ListColumn . mapM Csv.parseField $ B8.splitWith (==';') x
instance (Csv.ToField a, Show a) => Csv.ToField (ListColumn a) where
    toField x = B8.intercalate ";" $ map Csv.toField $ getListColumn x

instance Csv.DefaultOrdered TSVRow where
    headerOrder _ = Csv.header jannoHeader

jannoHeader :: [B8.ByteString]
jannoHeader = []
        
instance Csv.ToNamedRecord TSVRow where
    toNamedRecord j = explicitNA $ Csv.namedRecord [
          "Date_C14_Labnr"                  Csv..= _tsvRowC14Labnr j
        , "Date_C14_Uncal_BP"               Csv..= _tsvRowC14UncalBP j
        , "Date_C14_Uncal_BP_Err"           Csv..= _tsvRowC14UncalBPErr j
        , "Date_BC_AD_Start"                Csv..= _tsvRowBCADStart j
        , "Date_BC_AD_Median"               Csv..= _tsvRowBCADMedian j
        , "Date_BC_AD_Stop"                 Csv..= _tsvRowBCADStop j
        ] `HM.union` _otherColumns j

explicitNA :: Csv.NamedRecord -> Csv.NamedRecord
explicitNA = HM.map (\x -> if B8.null x then "n/a" else x)
