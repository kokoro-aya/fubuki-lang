module ParseTopLevel where

import ADT (Expr)
import FubukiParser (expr)
import Lexer (lexing)
import Parser (parse, Parser, eof)
import Display (display, Display)

type TopLevel = Expr

topLevel = expr <* eof

parseTopLevel :: String -> Expr
parseTopLevel = either (error . show) id . fst . parse topLevel . lexing

testParse :: Display a => Parser a -> String -> String
testParse p = either (\x -> "Syntax error: " ++ show x) display . fst . parse p . lexing

testParseShow :: Show a => Parser a -> String -> String
testParseShow p = either (\x -> "Syntax error: " ++ show x) show . fst . parse p . lexing