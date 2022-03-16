module ParseSymbols where
import Token (TokenType(DOT, LPAREN, RPAREN, COMMA, NOT, ADD, SUB, MUL, DIV, MOD, CARET, LSHIFT, RSHIFT, APPEND, THROUGH, UNTIL, DOWNTO, DOWNTHROUGH, GRT, LRT, LEQ, GEQ, STEP, EQU, NEQU, XOR, AND, OR, ADDEQ, SUBEQ, MULEQ, DIVEQ, MODEQ, ASSIGN), Token (tokenType), isCustomOperator, matchInfix)
import Parser (satisfy)


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

lesserthanSymbol = satisfy "expected token \"<\"" ((== LRT) . tokenType)

leqSymbol = satisfy "expected token \"<=\"" ((== LEQ) . tokenType)

greaterthanSymbol = satisfy "expected token \">\"" ((== GRT) . tokenType)

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

customSymbol = satisfy "expected token custom operator" (isCustomOperator . tokenType)

infix0 = satisfy "expected token infix operator of level 0" (matchInfix (`elem` "|$") . tokenType)

infix1 = satisfy "expected token infix operator of level 1" (matchInfix (== '&') . tokenType)

infix2 = satisfy "expected token infix operator of level 2" (matchInfix (`elem` "^@") . tokenType)

infix3 = satisfy "expected token infix operator of level 3" (matchInfix (== '=') . tokenType)

infix4 = satisfy "expected token infix operator of level 4" (matchInfix (`elem` "<>") . tokenType)

infix5 = satisfy "expected token infix operator of level 5" (matchInfix (`elem` "+-") . tokenType)

infix6 = satisfy "expected token infix operator of level 6" (matchInfix (`elem` "*/%") . tokenType)

prefix7 = satisfy "expected token prefix operator of level 7" (matchInfix (`elem` "!~") . tokenType)