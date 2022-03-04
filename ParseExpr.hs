module ParseExpr where

{-# LANGUAGE GADTs #-}
{-# LANGUAGE GADTs #-}
import Token (isLiteral, isReference, isOperator, Token (tokenType), TokenType)
import Parser (satisfy, sepBy1)
import ParseLiterals
    ( intLiteral, realLiteral, charLiteral, strLiteral, boolLiteral )
import Control.Applicative ((<|>))
import ParseSymbols (lparen, comma, rparen, notSymbol, addSymbol, subSymbol, mulSymbol, divSymbol, modSymbol)

data Op = Op { opType :: Token, opPrec :: Int }

data Expr = UnaryExpr Op Expr
          | BinaryExpr Op Expr Expr
          | PrimaryExpr Expr
          | Parenthesis Expr
          | TupleExpr [Expr]

data Primary = IntPrimary Int
             | RealPrimary Double
             | CharPrimary Char
             | StrPrimary String
             | BoolPrimary Bool

literalToken = satisfy "literal token expected" (isLiteral . tokenType)
referenceToken = satisfy "reference token expected" (isReference . tokenType)
literalOrReferenceToken = satisfy "literal or reference token expected" ((\x -> isLiteral x || isReference x) . tokenType)
operatorToken = satisfy "literal token expected" (isOperator . tokenType)


expr = primary


exprLevel2 = do
              t <- term
              (do
                  op <- addSymbol <|> subSymbol
                  BinaryExpr (Op op 2) t <$> exprLevel2)
                  <|> pure t

term = do
        s <- subterm
        (do
            op <- mulSymbol <|> divSymbol <|> modSymbol
            BinaryExpr (Op op 1) s <$> subterm)
            <|> pure s

subterm = do
            op <- notSymbol <|> addSymbol <|> subSymbol
            UnaryExpr (Op op 0) <$> factor
            <|> factor

factor = primary

primary = do
            lparen
            exprs <- sepBy1 expr comma
            rparen
            pure $ mkParenthesis exprs
            <|> PrimaryExpr <$> primary

mkParenthesis [x] = Parenthesis x
mkParenthesis xs = TupleExpr xs

subPrimary = literalPrimary

literalPrimary = do
                 IntPrimary <$> intLiteral
                 <|>
                     RealPrimary <$> realLiteral
                     <|>
                         CharPrimary <$> charLiteral
                         <|>
                             StrPrimary <$> strLiteral
                             <|>
                                 BoolPrimary <$> boolLiteral

