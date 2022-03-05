module ParseSymbols where
import Token (TokenType(DOT, LPAREN, RPAREN, COMMA, NOT, ADD, SUB, MUL, DIV, MOD, CARET, LSHIFT, RSHIFT, APPEND, THROUGH, UNTIL, DOWNTO, DOWNTHROUGH, GANGL, LANGL, LEQ, GEQ, STEP, EQU, NEQU, XOR, AND, OR, ADDEQ, SUBEQ, MULEQ, DIVEQ, MODEQ, ASSIGN), Token (tokenType), isCustomOperator)
import Parser (satisfy)


dot = satisfy "expected token \".\"" ((== DOT) . tokenType)

comma = satisfy "expected token \",\"" ((== COMMA) . tokenType)

lparen = satisfy "expected token \"(\"" ((== LPAREN) . tokenType)

rparen = satisfy "expected token \")\"" ((== RPAREN) . tokenType)

notSymbol = satisfy "expected token \"!\"" ((== NOT) . tokenType)

addSymbol = satisfy "expected token \"+\"" ((== ADD) . tokenType)

subSymbol = satisfy "expected token \"-\"" ((== SUB) . tokenType)

mulSymbol = satisfy "expected token \"*\"" ((== MUL) . tokenType)

divSymbol = satisfy "expected token \"/\"" ((== DIV) . tokenType)

modSymbol = satisfy "expected token \"%\"" ((== MOD) . tokenType)

caretSymbol = satisfy "expected token \"^\"" ((== CARET) . tokenType)

lshiftSymbol = satisfy "expected token \"<<\"" ((== LSHIFT) . tokenType)

rshiftSymbol = satisfy "expected token \">>\"" ((== RSHIFT) . tokenType)

appendSymbol = satisfy "expected token \"++\"" ((== APPEND) . tokenType)

untilSymbol = satisfy "expected token \"..<\"" ((== UNTIL) . tokenType)

throughSymbol = satisfy "expected token \"...\"" ((== THROUGH) . tokenType)

downtoSymbol = satisfy "expected token \">>.\"" ((== DOWNTO) . tokenType)

downthroughSymbol = satisfy "expected token \">..\"" ((== DOWNTHROUGH) . tokenType)

stepSymbol = satisfy "expected token \"@\"" ((== STEP) . tokenType)

lesserthanSymbol = satisfy "expected token \"<\"" ((== LANGL) . tokenType)

leqSymbol = satisfy "expected token \"<=\"" ((== LEQ) . tokenType)

greaterthanSymbol = satisfy "expected token \">\"" ((== GANGL) . tokenType)

geqSymbol = satisfy "expected token \">=\"" ((== GEQ) . tokenType)

eqSymbol = satisfy "expected token \"==\"" ((== EQU) . tokenType)

neqSymbol = satisfy "expected token \"!=\"" ((== NEQU) . tokenType)

xorSymbol = satisfy "expected token \"^^\"" ((== XOR) . tokenType)

andSymbol = satisfy "expected token \"&&\"" ((== AND) . tokenType)

orSymbol = satisfy "expected token \"||\"" ((== OR) . tokenType)

addeqSymbol = satisfy "expected token \"+=\"" ((== ADDEQ) . tokenType)

subeqSymbol = satisfy "expected token \"-=\"" ((== SUBEQ) . tokenType)

muleqSymbol = satisfy "expected token \"*=\"" ((== MULEQ) . tokenType)

diveqSymbol = satisfy "expected token \"/=\"" ((== DIVEQ) . tokenType)

modeqSymbol = satisfy "expected token \"%=\"" ((== MODEQ) . tokenType)

assignSymbol = satisfy "expected token \"=\"" ((== ASSIGN) . tokenType)

customSymbol = satisfy "expected token custom operator" (isCustomOperator . tokenType)