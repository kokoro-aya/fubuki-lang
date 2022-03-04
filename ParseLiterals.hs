module ParseLiterals where

import Parser (satisfy)
import Token (Token(tokenType), TokenType (Str, Numeric, Chr, FLS, TRU), literalValue, charLiteralValue)
import ParseSymbols (dot)
import Utils (readToInt, readToDouble)
import Control.Applicative ((<|>))

isNumeric (Numeric _) = True
isNumeric _ = False

int = satisfy "numeric value token expected" (isNumeric . tokenType)

isStr (Str _) = True
isStr _ = False

str = satisfy "string literal token expected" (isStr . tokenType)

isChr (Chr _) = True
isChr _ = False

chr = satisfy "character literal token expected" (isChr . tokenType)

intLiteral = readToInt . literalValue . tokenType <$> int

charLiteral = charLiteralValue . tokenType <$> chr

realLiteral = do
                left <- int
                dot
                right <- int
                pure $ readToDouble ((literalValue $ tokenType left) ++ "." ++ (literalValue $ tokenType right))

trueLiteral = satisfy "true literal token expected" ((== TRU) . tokenType)
falseLiteral = satisfy "false literal token expected" ((== FLS) . tokenType)

boolLiteral = do
                trueLiteral
                pure True
                <|> do
                    falseLiteral
                    pure False

strLiteral = literalValue . tokenType <$> str
