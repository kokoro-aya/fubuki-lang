module Fragments where

import Parser (satisfy)
import Token (Token(tokenType), TokenType (Str, Numeric, Chr, FLS, TRU, ULINE, Ident, COLUMN, LBRACKET, RBRACKET, ARROW, SLICE, CARET, SWITCH, LBRACE, RBRACE, LPAREN, RPAREN, COMMA, LAM_ARR), literalValue, charLiteralValue)
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

isIdentifier (Ident _) = True
isIdentifier _ = False

identifier = satisfy "variable token expected" (isIdentifier . tokenType)

wildcard = satisfy "wildcard token \"_\" expected" ((== ULINE) . tokenType)

column = satisfy "column token \":\" expected" ((== COLUMN) . tokenType)

lbracket = satisfy "left bracket token \"[\" expected" ((== LBRACKET) . tokenType)

rbracket = satisfy "right bracket token \"]\" expected" ((== RBRACKET) . tokenType)

lbrace = satisfy "left bracket token \"[\" expected" ((== LBRACE) . tokenType)

rbrace = satisfy "right bracket token \"]\" expected" ((== RBRACE) . tokenType)

lparen = satisfy "left bracket token \"[\" expected" ((== LPAREN) . tokenType)

rparen = satisfy "right bracket token \"]\" expected" ((== RPAREN) . tokenType)

arrow = satisfy "arrow token \"->\" expected" ((== ARROW) . tokenType)

slice = satisfy "slice token \"..\" expected" ((== SLICE) . tokenType)

caret = satisfy "caret token \"^\" expected" ((== CARET) . tokenType)

switch = satisfy "switch token \"switch\" expected" ((== SWITCH ) . tokenType)

comma = satisfy "comma token \",\" expected" ((== COMMA) . tokenType)

lamArr = satisfy "double arrow \"=>\" expected" ((== LAM_ARR) . tokenType)