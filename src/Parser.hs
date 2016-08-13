module Parser (parseBEncode) where

import Text.ParserCombinators.Parsec

import Types

bString :: Parser BAtom
bString = do
    num <- many1 digit
    char ':'
    word <- count (read num :: Int) anyChar
    return $ BString word

bInt :: Parser BAtom
bInt = do
    num <- char 'i' *> many1 digit <* char 'e'
    return $ BInt (read num :: Int)

bList :: Parser BAtom
bList = do
    atoms <- char 'l' *> many bValue <* char 'e'
    return $ BList atoms

bPair :: Parser (BAtom, BAtom)
bPair = do
    key <- bString
    val <- bValue
    return (key, val)

bDict :: Parser BAtom
bDict = do
    atoms <- char 'd' *> many bPair <* char 'e'
    return $ BDict atoms

bValue :: Parser BAtom
bValue = bString <|> bInt <|> bList <|> bDict

parseBEncode :: String -> Either ParseError BAtom
parseBEncode = parse bValue "(unknown)"
