{-# LANGUAGE OverloadedStrings #-}

module Currycarbon.TSV where

import Currycarbon.Types

import           Control.Applicative    (empty)
import qualified Data.ByteString.Char8  as B8
import qualified Data.ByteString.Lazy   as BL
import qualified Data.Csv               as Csv
import qualified Data.HashMap.Strict    as HM
import qualified Data.Text              as T
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
tsvRow2NamedCalExprs (TSVRow _ _ _ (Just start) _ (Just stop) _) =
    NamedCalExpr "" $  WindowBCAD (TimeWindowBCAD "" start stop)
tsvRow2NamedCalExprs (TSVRow _ _ _ _ _ _ _) = undefined

data TSV = TSV {
      _tsvFile :: FilePath
    , _tsvHeader :: Csv.Header
    , _tsvRows :: V.Vector TSVRow
    }

data TSVRow = TSVRow {
      _dateC14Labnr      :: Maybe (ListColumn T.Text)
    , _dateC14UncalBP    :: Maybe (ListColumn Int)
    , _dateC14UncalBPErr :: Maybe (ListColumn Int)
    , _dateBCADStart     :: Maybe Int
    , _dateBCADMedian    :: Maybe Int
    , _dateBCADStop      :: Maybe Int
    , _otherColumns      :: Csv.NamedRecord
    }
    deriving Show

instance Csv.FromNamedRecord TSVRow where
    parseNamedRecord m = do
        labnr      <- filterLookupOptional m "Date_C14_Labnr"
        uncalBP    <- filterLookupOptional m "Date_C14_Uncal_BP"
        uncalBPErr <- filterLookupOptional m "Date_C14_Uncal_BP_Err"
        start      <- filterLookupOptional m "Date_BC_AD_Start"
        median     <- filterLookupOptional m "Date_BC_AD_Median"
        stop       <- filterLookupOptional m "Date_BC_AD_Stop"
        pure $ TSVRow {
              _dateC14Labnr      = labnr
            , _dateC14UncalBP    = uncalBP
            , _dateC14UncalBPErr = uncalBPErr
            , _dateBCADStart     = start
            , _dateBCADMedian    = median
            , _dateBCADStop      = stop
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
          "Date_C14_Labnr"                  Csv..= _dateC14Labnr j
        , "Date_C14_Uncal_BP"               Csv..= _dateC14UncalBP j
        , "Date_C14_Uncal_BP_Err"           Csv..= _dateC14UncalBPErr j
        , "Date_BC_AD_Start"                Csv..= _dateBCADStart j
        , "Date_BC_AD_Median"               Csv..= _dateBCADMedian j
        , "Date_BC_AD_Stop"                 Csv..= _dateBCADStop j
        ] `HM.union` _otherColumns j

explicitNA :: Csv.NamedRecord -> Csv.NamedRecord
explicitNA = HM.map (\x -> if B8.null x then "n/a" else x)
