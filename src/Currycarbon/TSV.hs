{-# LANGUAGE OverloadedStrings #-}

module Currycarbon.TSV where

import           Currycarbon.Types
import           Currycarbon.Utils

import           Control.Applicative   (empty)
import           Control.Exception     (throwIO)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy  as BL
import           Data.Char             (ord)
import qualified Data.Csv              as Csv
import qualified Data.HashMap.Strict   as HM
import qualified Data.Vector           as V


-- turn .tsv files to NamedCalExprs for further processing


tsv2NamedCalExprs :: TSV -> [Either CurrycarbonException NamedCalExpr]
tsv2NamedCalExprs (TSV _ _ rows) = V.toList $ V.map tsvRow2NamedCalExprs rows

tsvRow2NamedCalExprs :: TSVRow -> Either CurrycarbonException NamedCalExpr
-- C14 age with n dates and a labcode
tsvRow2NamedCalExprs
    (TSVRow i (Just (ListColumn lcs)) (Just (ListColumn bps)) (Just (ListColumn errs)) _ _) |
    (length lcs == length bps) && (length bps == length errs) =
    Right $ NamedCalExpr i $ foldC14 $ zip3 lcs bps errs
-- C14 age with n dates and no labcode
tsvRow2NamedCalExprs
    (TSVRow i _ (Just (ListColumn bps)) (Just (ListColumn errs)) _ _) |
    length bps == length errs =
    Right $ NamedCalExpr i $ foldC14 $ zip3 (repeat "") bps errs
-- Contextual age
tsvRow2NamedCalExprs (TSVRow i _ _ _ (Just start) (Just stop)) =
    Right $ NamedCalExpr i $  WindowBCAD (TimeWindowBCAD "" start stop)
-- Error case if nothing fits
tsvRow2NamedCalExprs (TSVRow i _ _ _ _ _) =
    Left $ CurrycarbonTSV2CalExprException i

foldC14 :: [(String, Word, Word)] -> CalExpr
foldC14 xs = foldl1 SumCal $ map (\(lc,bp,err) -> UnCalDate $ UncalC14 lc bp err) xs

-- reading .tsv files


readTSV :: FilePath -> IO TSV
readTSV path = do
    bs <- BL.readFile path
    case Csv.decodeByNameWith decodingOptions bs of
      Left s -> throwIO $ CurrycarbonTSVParsingException s
      Right (header, rows) -> return (TSV path header rows)

decodingOptions :: Csv.DecodeOptions
decodingOptions = Csv.defaultDecodeOptions { Csv.decDelimiter = fromIntegral (ord '\t') }

data TSV = TSV {
      _tsvFile   :: FilePath
    , _tsvHeader :: Csv.Header
    , _tsvRows   :: V.Vector TSVRow
    }

data TSVRow = TSVRow {
      _tsvRowID                :: String
    , _tsvRowDateC14Labnr      :: Maybe (ListColumn String)
    , _tsvRowDateC14UncalBP    :: Maybe (ListColumn Word)
    , _tsvRowDateC14UncalBPErr :: Maybe (ListColumn Word)
    , _tsvRowDateBCADStart     :: Maybe Int
    , _tsvRowDateBCADStop      :: Maybe Int
    -- for any other columns
    --, _tsvRowAllColumns        :: Csv.NamedRecord
    }
    deriving Show

instance Csv.FromNamedRecord TSVRow where
    parseNamedRecord m = do
        i          <- filterLookup m "Date_ID"
        labnr      <- filterLookupOptional m "Date_C14_Labnr"
        uncalBP    <- filterLookupOptional m "Date_C14_Uncal_BP"
        uncalBPErr <- filterLookupOptional m "Date_C14_Uncal_BP_Err"
        start      <- filterLookupOptional m "Date_BC_AD_Start"
        stop       <- filterLookupOptional m "Date_BC_AD_Stop"
        pure $ TSVRow {
              _tsvRowID                = i
            , _tsvRowDateC14Labnr      = labnr
            , _tsvRowDateC14UncalBP    = uncalBP
            , _tsvRowDateC14UncalBPErr = uncalBPErr
            , _tsvRowDateBCADStart     = start
            , _tsvRowDateBCADStop      = stop
            -- for any other columns
            --, _tsvRowAllColumns        = m
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
