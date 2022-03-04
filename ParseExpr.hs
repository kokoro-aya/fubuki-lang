{-# LANGUAGE GADTs #-}
import Token (isLiteral, isReference, isOperator, Token (tokenType))
import Parser (satisfy)
import ParseLiterals
    ( intLiteral, realLiteral, charLiteral, strLiteral, boolLiteral )
import Control.Applicative ((<|>))

data Op = Op {
    opName :: String,
    opPrec :: Int
}

data Expr a where
    UnaryExpr :: Op -> Expr a -> Expr a
    BinaryExpr :: Op -> Expr a -> Expr a -> Expr a
    PrimaryExpr :: Primary -> Expr Primary

data Primary = IntPrimary Int
             | RealPrimary Double
             | CharPrimary Char
             | StrPrimary String
             | BoolPrimary Bool

literalToken = satisfy "literal token expected" (isLiteral . tokenType)
referenceToken = satisfy "reference token expected" (isReference . tokenType)
literalOrReferenceToken = satisfy "literal or reference token expected" ((\x -> isLiteral x || isReference x) . tokenType)
operatorToken = satisfy "literal token expected" (isOperator . tokenType)




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

