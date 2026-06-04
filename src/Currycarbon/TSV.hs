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

readTSV :: FilePath -> IO (Csv.Header, V.Vector TSVRow)
readTSV path = do
    bs <- BL.readFile path
    case Csv.decodeByNameWith (Csv.DecodeOptions $ fromIntegral (ord '\t')) bs of
      Left err -> fail err
      Right (header, rows) -> return (header, rows)

tsvRow2NamedCalExpr :: TSVRow -> CalExpr
tsvRow2NamedCalExpr (TSVRow _ _ _ (Just start) _ (Just stop) _) = WindowBCAD (TimeWindowBCAD "" start stop)
tsvRow2NamedCalExpr (TSVRow _ _ _ _ _ _ _) = undefined

data TSVRow = TSVRow {
      jDateC14Labnr      :: ListColumn T.Text
    , jDateC14UncalBP    :: ListColumn Int
    , jDateC14UncalBPErr :: ListColumn Int
    , jDateBCADStart     :: Maybe Int
    , jDateBCADMedian    :: Maybe Int
    , jDateBCADStop      :: Maybe Int
    , jOtherColumns      :: Csv.NamedRecord
    }
    deriving Show

instance Csv.FromNamedRecord TSVRow where
    parseNamedRecord m = do
        labnr      <- filterLookup m "Date_C14_Labnr"
        uncalBP    <- filterLookup m "Date_C14_Uncal_BP"
        uncalBPErr <- filterLookup m "Date_C14_Uncal_BP_Err"
        start      <- filterLookupOptional m "Date_BC_AD_Start"
        median     <- filterLookupOptional m "Date_BC_AD_Median"
        stop       <- filterLookupOptional m "Date_BC_AD_Stop"
        pure $ TSVRow {
              jDateC14Labnr      = labnr
            , jDateC14UncalBP    = uncalBP
            , jDateC14UncalBPErr = uncalBPErr
            , jDateBCADStart     = start
            , jDateBCADMedian    = median
            , jDateBCADStop      = stop
            , jOtherColumns      = m
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
          "Date_C14_Labnr"                  Csv..= jDateC14Labnr j
        , "Date_C14_Uncal_BP"               Csv..= jDateC14UncalBP j
        , "Date_C14_Uncal_BP_Err"           Csv..= jDateC14UncalBPErr j
        , "Date_BC_AD_Start"                Csv..= jDateBCADStart j
        , "Date_BC_AD_Median"               Csv..= jDateBCADMedian j
        , "Date_BC_AD_Stop"                 Csv..= jDateBCADStop j
        ] `HM.union` jOtherColumns j

explicitNA :: Csv.NamedRecord -> Csv.NamedRecord
explicitNA = HM.map (\x -> if B8.null x then "n/a" else x)
