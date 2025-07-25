module Currycarbon.ParserHelpers where

import qualified Text.Parsec        as P
import qualified Text.Parsec.Error  as P
import qualified Text.Parsec.String as P

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

parseArgumentWithDefault :: String -> P.Parser b -> b -> P.Parser b
parseArgumentWithDefault argumentName parseValue defaultValue =
    P.option defaultValue (parseArgument argumentName parseValue)

parseArgumentOptional :: String -> P.Parser b -> P.Parser (Maybe b)
parseArgumentOptional argumentName parseValue =
    P.optionMaybe $ P.try (parseArgument argumentName parseValue)

parseArgument :: String -> P.Parser b -> P.Parser b
parseArgument argumentName parseValue = do
    res <- parseArgumentWithoutComma argumentName parseValue
    P.optional consumeCommaSep
    return res

parseNamedArgumentOptional :: String -> P.Parser b -> P.Parser (Maybe b)
parseNamedArgumentOptional argumentName parseValue =
    P.optionMaybe $ P.try (parseNamedArgument argumentName parseValue)

-- * Low level blocks

parseArgumentWithoutComma :: String -> P.Parser b -> P.Parser b
parseArgumentWithoutComma argumentName parseValue =
    P.try (parseNamedArgument argumentName parseValue) P.<|> parseUnnamedArgument parseValue

parseNamedArgument :: String -> P.Parser b -> P.Parser b
parseNamedArgument argumentName parseValue = do
    (_,b) <- parseKeyValuePair (P.string argumentName) parseValue
    return b

parseUnnamedArgument :: P.Parser b -> P.Parser b
parseUnnamedArgument parseValue = parseValue

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
        inNoQuotes = P.many (P.noneOf ",):")

-- * Sequence parsers

parseDoubleSequence :: P.Parser [Double]
parseDoubleSequence = do
    start <- parseDouble
    _ <- P.oneOf ":"
    stop <- parseDouble
    _ <- P.oneOf ":"
    by <- parsePositiveDouble
    return [start,(start+by)..stop]

-- * Number parsers

parseDouble :: P.Parser Double
parseDouble = do
    P.try parseNegativeDouble P.<|> parsePositiveDouble

parseNegativeDouble :: P.Parser Double
parseNegativeDouble = do
    _ <- P.oneOf "-"
    i <- parsePositiveDouble
    return (-i)

parseFraction :: P.Parser Double
parseFraction = do
    num <- parsePositiveDouble
    if num > 1
    then fail "must be between zero and one"
    else return num

parsePositiveDouble :: P.Parser Double
parsePositiveDouble = do
    num <- parseNumber
    optionalMore <- P.option "" $ (:) <$> P.char '.' <*> parseNumber
    return $ read $ num ++ optionalMore

parseIntegerSequence :: P.Parser [Int]
parseIntegerSequence = do
    start <- parseInt
    _ <- P.oneOf ":"
    stop <- parseInt
    _ <- P.oneOf ":"
    by <- fromIntegral <$> parsePositiveInt
    return [start,(start+by)..stop]

parseInt :: P.Parser Int
parseInt = do
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

parseNumber :: P.Parser [Char]
parseNumber = P.many1 P.digit

-- * Error helpers

showParsecErr :: P.ParseError -> String
showParsecErr err =
    let pos = P.errorPos err
        posStr = P.sourceName pos ++ ":" ++ show (P.sourceLine pos) ++ ":" ++ show (P.sourceColumn pos)
        errMsg = P.showErrorMessages
            "or" "unknown parse error"
            "expecting" "unexpected" "end of input"
            (P.errorMessages err)
    in posStr ++ ": " ++ errMsg
