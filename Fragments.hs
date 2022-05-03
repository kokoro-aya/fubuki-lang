module Fragments where

import Parser (satisfy)
import Token (Token(tokenType), TokenType (Str, Numeric, Chr, FLS, TRU, ULINE, Ident, COLUMN, LBRACKET, RBRACKET, ARROW, SLICE, CARET, SWITCH, LBRACE, RBRACE, LPAREN, RPAREN, COMMA, LAM_ARR, SEMI, FOR, IN, WHILE, REPEAT, IF, ELSE, CASE, DEFAULT, BREAK, CONTINUE, FALLTHRU, RETURN, VAL, VAR, FN, ASSIGN, DOT, DOUBLE_COLUMN, AMP, QMARK, DO, BACKTICK, LRT, LEQ, GRT, GEQ, GENERIC_LEFT, GENERIC_RIGHT, NOP), literalValue, charLiteralValue)
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

uline = satisfy "wildcard token \"_\" expected" ((== ULINE) . tokenType)

column = satisfy "column token \":\" expected" ((== COLUMN) . tokenType)

ampersand = satisfy "ampersand token \"&\" expected" ((== AMP) . tokenType)

qmark = satisfy "question mark token \"?\" expected" ((== QMARK) . tokenType)

doubleColumn = satisfy "double column token \"::\" expected" ((== DOUBLE_COLUMN) . tokenType)

lbracket = satisfy "left bracket token \"[\" expected" ((== LBRACKET) . tokenType)

rbracket = satisfy "right bracket token \"]\" expected" ((== RBRACKET) . tokenType)

lbrace = satisfy "left bracket token \"{\" expected" ((== LBRACE) . tokenType)

rbrace = satisfy "right bracket token \"}\" expected" ((== RBRACE) . tokenType)

lparen = satisfy "left bracket token \"(\" expected" ((== LPAREN) . tokenType)

rparen = satisfy "right bracket token \")\" expected" ((== RPAREN) . tokenType)

assign = satisfy "expected token \"=\"" ((== ASSIGN) . tokenType)

arrow = satisfy "arrow token \"->\" expected" ((== ARROW) . tokenType)

slice = satisfy "slice token \"..\" expected" ((== SLICE) . tokenType)

dot = satisfy "expected token \".\"" ((== DOT) . tokenType)

comma = satisfy "expected token \",\"" ((== COMMA) . tokenType)

lamArr = satisfy "double arrow \"=>\" expected" ((== LAM_ARR) . tokenType)

semicolon = satisfy "semicolon token \";\" expected" ((== SEMI) . tokenType)

for = satisfy "for token \"for\" expected" ((== FOR) . tokenType)

in_ = satisfy "in token \"in\" expected" ((== IN) . tokenType)

while = satisfy "while token \"while\" expected" ((== WHILE) . tokenType)

repeat_ = satisfy "repeat token \"repeat\" expected" ((== REPEAT) . tokenType)

if_ = satisfy "if token \"if\" expected" ((== IF) . tokenType)

else_ = satisfy "else token \"else\" expected" ((== ELSE) . tokenType)

switch = satisfy "switch token \"switch\" expected" ((== SWITCH) . tokenType)

case_ = satisfy "case token \"case\" expected" ((== CASE) . tokenType)

default_ = satisfy "default token \"default\" expected" ((== DEFAULT) . tokenType)

break_ = satisfy "break token \"break\" expected" ((== BREAK) . tokenType)

continue_ = satisfy "continue token \"continue\" expected" ((== CONTINUE) . tokenType)

fallthrough = satisfy "fallthrough token \":||\" expected" ((== FALLTHRU) . tokenType)

retn = satisfy "return token \"return\" expected" ((== RETURN) . tokenType)

val = satisfy "val token \"val\" expected" ((== VAL) . tokenType)

var = satisfy "var token \"var\" expected" ((== VAR) . tokenType)

fn = satisfy "fn token \"fn\" expected" ((== FN) . tokenType)

do_ = satisfy "do token \"do\" expected" ((== DO) . tokenType)

backtick = satisfy "backtick token \"`\" expected" ((== BACKTICK) . tokenType)

lesserthanSymbol = satisfy "expected token \"<\"" ((== LRT) . tokenType)

leqSymbol = satisfy "expected token \"<=\"" ((== LEQ) . tokenType)

greaterthanSymbol = satisfy "expected token \">\"" ((== GRT) . tokenType)

geqSymbol = satisfy "expected token \">=\"" ((== GEQ) . tokenType)

genericLeft = satisfy "expected generic token \"<\"" ((== GENERIC_LEFT) . tokenType)

genericRight = satisfy "expected generic token \">\"" ((== GENERIC_RIGHT) . tokenType)



nop = satisfy "this token is not allowed" ((== NOP) . tokenType)