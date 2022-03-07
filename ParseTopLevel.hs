module ParseTopLevel where

import ADT (Expr)
import FubukiParser (expr)
import Lexer (lexing)
import Parser (parse)
import Display (display)

type TopLevel = Expr

topLevel = expr

parseTopLevel :: String -> Expr
parseTopLevel = either (error . show) fst . parse topLevel . lexing

testParseTopLevel :: String -> String
testParseTopLevel = either (\x -> "Syntax error: " ++ show x) (display . fst) . parse topLevel . lexing