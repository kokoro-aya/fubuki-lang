module ParseSymbols where
import Token (TokenType(DOT), Token (tokenType))
import Parser (satisfy)

isDot DOT = True
isDot _  = False

dot = satisfy "expected token \".\"" (isDot . tokenType)

