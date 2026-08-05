{-# LANGUAGE OverloadedStrings #-}

module Currycarbon.TSV where

import           Currycarbon.ParserHelpers
import           Currycarbon.Types
import           Currycarbon.Utils

import           Control.Applicative       (empty)
import           Control.Exception         (throwIO)
import qualified Data.ByteString.Char8     as B8
import qualified Data.ByteString.Lazy      as BL
import           Data.Char                 (ord)
import qualified Data.Csv                  as Csv
import qualified Data.HashMap.Strict       as HM
import           Data.Maybe                (catMaybes)
import qualified Data.Vector               as V
import qualified Text.Parsec               as P
import qualified Text.Parsec.String        as P

-- | Turn 'TSV' to 'NamedCalExpr's for further processing (see 'evalNamedCalExpr')
tsv2NamedCalExprs :: CombinationStrategy -> TSV -> [Either CurrycarbonException NamedCalExpr]
tsv2NamedCalExprs combStrat (TSV _ _ rows) = V.toList $ V.map (tsvRow2NamedCalExprs combStrat) rows

tsvRow2NamedCalExprs :: CombinationStrategy -> TSVRow -> Either CurrycarbonException NamedCalExpr
-- C14 age with n dates and a labcode
tsvRow2NamedCalExprs combStrat
    (TSVRow i (Just (ListColumn lcs)) (Just (ListColumn bps)) (Just (ListColumn errs)) _ _) |
    (length lcs == length bps) && (length bps == length errs) =
    Right $ NamedCalExpr i $ foldC14 combStrat $ zip3 lcs bps errs
-- C14 age with n dates and no labcode
tsvRow2NamedCalExprs combStrat
    (TSVRow i _ (Just (ListColumn bps)) (Just (ListColumn errs)) _ _) |
    length bps == length errs =
    Right $ NamedCalExpr i $ foldC14 combStrat $ zip3 (repeat "") bps errs
-- contextual age
tsvRow2NamedCalExprs _ (TSVRow i _ _ _ (Just start) (Just stop)) =
   NamedCalExpr i . WindowBCAD <$> makeTimeWindowBCAD "" start stop
-- error case if nothing fits
tsvRow2NamedCalExprs _ (TSVRow i _ _ _ _ _) =
    Left $ CurrycarbonTSV2CalExprException i

-- | A data type for the strategy to combine multiple C14 dates.
data CombinationStrategy =
     CombSum -- ^ Form a normalised sum of the 'CalPDF's.
   | CombProduct -- ^ Form a normalised product of the 'CalPDF's.

instance Show CombinationStrategy where
    show CombSum     = "Sum"
    show CombProduct = "Product"

readCombinationStrategy :: String -> Either String CombinationStrategy
readCombinationStrategy s =
    case P.runParser parseCombinationStrategy () s s of
        Left err -> Left $ showParsecErrOneLine err
        Right x  -> Right x

parseCombinationStrategy :: P.Parser CombinationStrategy
parseCombinationStrategy =  do
    x <- P.many P.anyChar
    case x of
        "Sum"     -> pure CombSum
        "Product" -> pure CombProduct
        _         -> fail "must be either Sum or Product"

foldC14 :: CombinationStrategy -> [(String, Word, Word)] -> CalExpr
foldC14 CombSum xs     = foldl1 SumCal     $ map (\(lc,bp,err) -> UnCalDate $ UncalC14 lc bp err) xs
foldC14 CombProduct xs = foldl1 ProductCal $ map (\(lc,bp,err) -> UnCalDate $ UncalC14 lc bp err) xs

-- | Read calibration expressions from a .tsv file. The file should have the following columns:
--
-- @
-- Date_ID	Date_C14_Labnr	Date_C14_Uncal_BP	Date_C14_Uncal_BP_Err	Date_BC_AD_Start	Date_BC_AD_Median	Date_BC_AD_Stop
-- Sample1				-200		100
-- Sample2	TEST-1	1000	30
-- Sample3	TEST-2;TEST-3	3000;3200	30;50
-- @
--
-- Subsets like <Date_ID> + <Date_C14_Uncal_BP> + <Date_C14_Uncal_BP_Err> or
-- <Date_ID> + <Date_BC_AD_Start> + <Date_BC_AD_Stop> are also sufficient.
-- The structure is inspired by the Poseidon .janno file.
--
readTSV :: FilePath -> IO TSV
readTSV path = do
    bs <- BL.readFile path
    case Csv.decodeByNameWith decodingOptions bs of
      Left s               -> throwIO $ CurrycarbonTSVParsingException s
      Right (header, rows) -> return (TSV path header rows)

decodingOptions :: Csv.DecodeOptions
decodingOptions = Csv.defaultDecodeOptions { Csv.decDelimiter = fromIntegral (ord '\t') }

-- | A data type for tab-separated (.tsv) files
data TSV = TSV {
      _tsvFile   :: FilePath
    , _tsvHeader :: Csv.Header
    , _tsvRows   :: V.Vector TSVRow
    }

-- | A data type to represent the relevant fields of one row in a .tsv file
data TSVRow = TSVRow {
    -- | Identifier of the expression
      _tsvRowID                :: String
    -- | Lab codes of radiocarbon dates
    , _tsvRowDateC14Labnr      :: Maybe (ListColumn String)
    -- | C14 ages in years BP
    , _tsvRowDateC14UncalBP    :: Maybe (ListColumn Word)
    -- | C14 standard deviations (one sigma in years)
    , _tsvRowDateC14UncalBPErr :: Maybe (ListColumn Word)
    -- | Start of time window in years BC
    , _tsvRowDateBCADStart     :: Maybe Int
    -- | End of time window in years BC
    , _tsvRowDateBCADStop      :: Maybe Int
    -- for any other columns
    -- , _tsvRowAllColumns        :: Csv.NamedRecord
    }
    deriving Show

instance Csv.FromNamedRecord TSVRow where
    parseNamedRecord m = do
        i          <- filterLookupMulti m ["Date_ID", "Poseidon_ID"]
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
            -- , _tsvRowAllColumns        = m
            }

-- lookup column by name
filterLookup :: Csv.FromField a => Csv.NamedRecord -> B8.ByteString -> Csv.Parser a
filterLookup m name = maybe empty Csv.parseField $ cleanInput $ HM.lookup name m

-- lookup optional column by name
filterLookupOptional :: Csv.FromField a => Csv.NamedRecord -> B8.ByteString -> Csv.Parser (Maybe a)
filterLookupOptional m name = maybe (pure Nothing) (\bs -> Just <$> Csv.parseField bs) $
                              cleanInput $ HM.lookup name m

-- lookup column by multiple different names and keep the first match
filterLookupMulti :: Csv.FromField a => Csv.NamedRecord -> [B8.ByteString] -> Csv.Parser a
filterLookupMulti m names =
    maybe empty Csv.parseField $ cleanInput $ lookupMulti names
    where
        lookupMulti :: [B8.ByteString] -> Maybe B8.ByteString
        lookupMulti ns =
            let vals = map (`HM.lookup` m) ns
            in case catMaybes vals of
                []    -> Nothing
                (x:_) -> Just x

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
