module Currycarbon.Parsers where

import Currycarbon.Types
import Currycarbon.Utils

import           Control.Exception              (throwIO)
import           Control.Monad                  (guard)
import qualified Text.Parsec                    as P
import qualified Text.Parsec.String             as P
import qualified Text.Parsec.Number             as P

writeCalPDF :: CalPDF -> IO ()
writeCalPDF (CalPDF obs) =
    writeFile "test.txt" $ concatMap (\(year,prob) -> show year ++ "," ++ show prob ++ "\n") obs

readCalCurve :: IO CalCurve 
readCalCurve = do
    intcal20 <- readFile "data/intcal20.14c"
    case P.runParser calCurveFileParser () "" intcal20 of
        Left p  -> error $ "This should never happen." ++ show p
        Right x -> return $ CalCurve x

calCurveFileParser :: P.Parser [(Int, Int, Int)]
calCurveFileParser = do
    P.skipMany comments
    P.sepEndBy calCurveLineParser (P.manyTill P.anyToken (P.try P.newline))

calCurveLineParser :: P.Parser (Int, Int, Int) 
calCurveLineParser = do
  calBP <- read <$> P.many1 P.digit
  _ <- P.oneOf ","
  bp <- read <$> P.many1 P.digit
  _ <- P.oneOf ","
  sigma <- read <$> P.many1 P.digit
  return (bp, calBP, sigma)

comments :: P.Parser String
comments = do 
    P.string "#"
    comment <- P.manyTill P.anyChar P.newline
    return ""