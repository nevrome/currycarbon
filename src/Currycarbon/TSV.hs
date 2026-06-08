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
    case Csv.decodeByNameWith decodingOptions bs of
      Left err -> fail err
      Right (header, rows) -> return (TSV path header rows)

decodingOptions :: Csv.DecodeOptions
decodingOptions = Csv.defaultDecodeOptions {
    Csv.decDelimiter = fromIntegral (ord '\t')
}

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

writeTSVFile :: FilePath -> Maybe Csv.Header -> V.Vector TSVRow -> IO ()
writeTSVFile path maybeHeader rows = do
    let rowsAsBytestring = Csv.encodeByNameWith encodingOptions (makeHeader maybeHeader) $ V.toList rows
    BL.writeFile path rowsAsBytestring

encodingOptions :: Csv.EncodeOptions
encodingOptions = Csv.defaultEncodeOptions {
      Csv.encDelimiter = fromIntegral (ord '\t')
    , Csv.encUseCrLf = False
    , Csv.encIncludeHeader = True
    , Csv.encQuoting = Csv.QuoteMinimal
}

makeHeader :: Maybe Csv.Header -> Csv.Header
makeHeader Nothing = V.fromList [
      "Date_ID",
      "Date_C14_Labnr", "Date_C14_Uncal_BP", "Date_C14_Uncal_BP_Err"
    , "Date_BC_AD_Start", "Date_BC_AD_Median", "Date_BC_AD_Stop"
    ]

data TSV = TSV {
      _tsvFile :: FilePath
    , _tsvHeader :: Csv.Header
    , _tsvRows :: V.Vector TSVRow
    }

calPDF2TSVRow :: CalC14 -> TSVRow

data TSVRow = TSVRow {
      _tsvRowID            :: String
    , _tsvRowDateC14Labnr      :: Maybe (ListColumn String)
    , _tsvRowDateC14UncalBP    :: Maybe (ListColumn Word)
    , _tsvRowDateC14UncalBPErr :: Maybe (ListColumn Word)
    , _tsvRowDateBCADStart     :: Maybe Int
    , _tsvRowDateBCADMedian    :: Maybe Int
    , _tsvRowDateBCADStop      :: Maybe Int
    , _tsvRowAllColumns    :: Csv.NamedRecord
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
            , _tsvRowDateC14Labnr      = labnr
            , _tsvRowDateC14UncalBP    = uncalBP
            , _tsvRowDateC14UncalBPErr = uncalBPErr
            , _tsvRowDateBCADStart     = start
            , _tsvRowDateBCADMedian    = median
            , _tsvRowDateBCADStop      = stop
            , _tsvRowAllColumns    = m
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
          "Date_BC_AD_Start"      Csv..= _tsvRowDateBCADStart j
        , "Date_BC_AD_Median"     Csv..= _tsvRowDateBCADMedian j
        , "Date_BC_AD_Stop"       Csv..= _tsvRowDateBCADStop j
        ] `HM.union` _tsvRowAllColumns j
        -- from the unordered-containers documentation:
        -- If a key occurs in both maps, the mapping from the first will be the mapping in the result.
        -- that means that the input values will be overwritten by these values

explicitNA :: Csv.NamedRecord -> Csv.NamedRecord
explicitNA = HM.map (\x -> if B8.null x then "n/a" else x)
