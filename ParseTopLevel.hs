module ParseTopLevel where
import ParseExprsPatterns (Expr, expr)
import Lexer (lexing)
import Parser (parse)

type TopLevel = Expr

topLevel = expr

parseTopLevel :: String -> Expr
parseTopLevel = either (error . show) fst . parse topLevel . lexing

testParseTopLevel :: String -> String
testParseTopLevel = either (\x -> "Syntax error: " ++ show x) (show . fst) . parse topLevel . lexing