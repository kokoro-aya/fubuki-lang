module TestBack where
import Parser (chainl1, parse, endOptional)
import Fragments (int, semicolon)
import ParseSymbols (addSymbol)
import ADT (Expr(BinaryExpr, PrimaryExpr), Primary (IntPrimary))
import Token (literalValue, Token (tokenType), TokenType (ADD))
import Lexer
import Display

addExpr = chainl1
            (int >>= \x -> pure (PrimaryExpr (IntPrimary (read (literalValue . tokenType $ x):: Int))))
            (do a <- addSymbol
                return $ \x y ->  BinaryExpr ADD x y)

test p = parse p . lexing

endopt e = endOptional e semicolon