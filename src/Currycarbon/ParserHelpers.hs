module Currycarbon.ParserHelpers where

import qualified Text.Parsec              as P
import qualified Text.Parsec.Error        as P
import qualified Text.Parsec.String       as P

-- * High level building blocks

parseRecordType :: String -> P.Parser a -> P.Parser a
parseRecordType typeName parser = do
    _ <- P.string typeName
    parseInParens parser

parseNamedVector :: P.Parser a -> P.Parser b -> P.Parser [(a,b)]
parseNamedVector parseKey parseValue =
    parseVector $ parseKeyValuePair parseKey parseValue

parseVector :: P.Parser a -> P.Parser [a]
parseVector parser = do
    _ <- P.char 'c'
    parseInParens (P.sepBy parser consumeCommaSep)

-- * Low level blocks

parseArgumentOptional :: String -> P.Parser b -> P.Parser (Maybe b)
parseArgumentOptional argumentName parseValue =
    P.optionMaybe $ P.try (parseArgument argumentName parseValue)

parseArgument :: String -> P.Parser b -> P.Parser b
parseArgument argumentName parseValue = do
    res <- parseArgumentWithoutComma argumentName parseValue
    P.optional consumeCommaSep
    return res

parseArgumentWithoutComma :: String -> P.Parser b -> P.Parser b
parseArgumentWithoutComma argumentName parseValue =
    P.try parseNamedArgument P.<|> parseUnnamedArgument
    where
        parseNamedArgument = do
            (_,b) <- parseKeyValuePair (P.string argumentName) parseValue
            return b
        parseUnnamedArgument = parseValue

parseKeyValuePair :: P.Parser a -> P.Parser b -> P.Parser (a,b)
parseKeyValuePair parseKey parseValue = do
    key <- parseKey
    consumeEqualSep
    value <- parseValue
    return (key, value)

parseInParens :: P.Parser b -> P.Parser b
parseInParens parser = do
    _ <- P.char '('
    _ <- P.spaces
    res <- parser
    _ <- P.spaces
    _ <- P.char ')'
    return res

consumeEqualSep :: P.Parser ()
consumeEqualSep = do
    _ <- P.spaces *> P.char '=' <* P.spaces
    return ()
consumeCommaSep :: P.Parser ()
consumeCommaSep = do
    _ <- P.spaces *> P.char ',' <* P.spaces
    return ()

parseCharInSpace :: Char -> P.Parser Char
parseCharInSpace c = P.between P.spaces P.spaces (P.char c)

parseAnyString :: P.Parser String
parseAnyString =
    P.try inDoubleQuotes P.<|> P.try inSingleQuotes P.<|> inNoQuotes
    where
        inDoubleQuotes = P.between (P.char '"') (P.char '"') (P.many P.anyChar)
        inSingleQuotes = P.between (P.char '\'') (P.char '\'') (P.many P.anyChar)
        inNoQuotes = P.many (P.noneOf ",")

-- * Sequence parsers

parseDoubleSequence :: P.Parser [Double]
parseDoubleSequence = do
    start <- parseDouble
    _ <- P.oneOf ":"
    stop <- parseDouble
    _ <- P.oneOf ":"
    by <- parsePositiveFloatNumber
    return [start,(start+by)..stop]

-- * Number parsers

parseDouble :: P.Parser Double
parseDouble = do
    P.try parseNegativeFloatNumber P.<|> parsePositiveFloatNumber

parseNegativeFloatNumber :: P.Parser Double
parseNegativeFloatNumber = do
    _ <- P.oneOf "-"
    i <- parsePositiveFloatNumber
    return (-i)

parseFraction :: P.Parser Double
parseFraction = do
    num <- parsePositiveFloatNumber
    if num > 1
    then fail "must be between zero and one"
    else return num

parsePositiveFloatNumber :: P.Parser Double
parsePositiveFloatNumber = do
    num <- parseNumber
    optionalMore <- P.option "" $ (:) <$> P.char '.' <*> parseNumber
    return $ read $ num ++ optionalMore

parseIntegerSequence :: P.Parser [Int]
parseIntegerSequence = do
    start <- parseInteger
    _ <- P.oneOf ":"
    stop <- parseInteger
    _ <- P.oneOf ":"
    by <- fromIntegral <$> parsePositiveInt
    return [start,(start+by)..stop]

parseInteger :: P.Parser Int
parseInteger = do
    P.try parseNegativeInt P.<|> parsePositiveInt

parseNegativeInt :: P.Parser Int
parseNegativeInt = do
    _ <- P.oneOf "-"
    i <- parsePositiveInt
    return (-i)

parsePositiveInt :: P.Parser Int
parsePositiveInt = fromIntegral <$> parseWord

-- https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Word.html
parseWord :: P.Parser Word
parseWord = do
    read <$> parseNumber

parsePositiveDouble :: P.Parser Double
parsePositiveDouble = do
    read <$> parseNumber

parseNumber :: P.Parser [Char]
parseNumber = P.many1 P.digit

-- * Error helpers

showParsecErr :: P.ParseError -> String
showParsecErr err =
    P.showErrorMessages
        "or" "unknown parse error"
        "expecting" "unexpected" "end of input"
        (P.errorMessages err)
