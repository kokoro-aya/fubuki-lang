module FubukiParser where

import Token (isLiteral, isReference, isOperator, Token (tokenType), TokenType (Ident), identifierName)
import Parser (satisfy, sepBy1)
import ADT
import Fragments
    ( intLiteral, realLiteral, charLiteral, strLiteral, boolLiteral, wildcard, identifier )
import Control.Applicative ((<|>))
import ParseSymbols (lparen, comma, rparen, notSymbol, addSymbol, subSymbol, mulSymbol, divSymbol, modSymbol, caretSymbol, lshiftSymbol, rshiftSymbol, appendSymbol, throughSymbol, untilSymbol, downtoSymbol, downthroughSymbol, stepSymbol, lesserthanSymbol, greaterthanSymbol, leqSymbol, geqSymbol, eqSymbol, neqSymbol, xorSymbol, andSymbol, orSymbol, addeqSymbol, subeqSymbol, muleqSymbol, assignSymbol, diveqSymbol, modeqSymbol, infix0, infix1, infix2, infix3, infix4, infix5, infix6, prefix7)

literalToken = satisfy "literal token expected" (isLiteral . tokenType)
referenceToken = satisfy "reference token expected" (isReference . tokenType)
literalOrReferenceToken = satisfy "literal or reference token expected" ((\x -> isLiteral x || isReference x) . tokenType)
operatorToken = satisfy "literal token expected" (isOperator . tokenType)

----------------------------
-- Parser for expressions --
----------------------------


expr = exprLevel9

exprLevel9 = do
              p <- pattern_
              op <- addeqSymbol <|> subeqSymbol <|> muleqSymbol <|> diveqSymbol <|> modeqSymbol <|> assignSymbol
              AssignedExpr (Op op 12) p <$> exprLevel8
              <|> exprLevel8

exprLevel8 = do
              e1 <- exprLevel7
              (do op <- orSymbol <|> infix0
                  BinaryExpr (Op op 11) e1 <$> exprLevel8)
                  <|> pure e1

exprLevel7 = do
              e1 <- exprLevel6
              (do op <- andSymbol <|> infix1
                  BinaryExpr (Op op 10) e1 <$> exprLevel7)
                  <|> pure e1

exprLevel6 = do
              e1 <- exprLevel5
              (do op <- xorSymbol <|> infix2
                  BinaryExpr (Op op 9) e1 <$> exprLevel6)
                  <|> pure e1

exprLevel5 = do
              e1 <- exprLevel4
              (do op <- eqSymbol <|> neqSymbol <|> infix3
                  BinaryExpr (Op op 8) e1 <$> exprLevel5)
                  <|> pure e1

exprLevel4 = do
              e1 <- exprLevel3
              (do op <- lesserthanSymbol <|> greaterthanSymbol <|> leqSymbol <|> geqSymbol <|> infix4
                  BinaryExpr (Op op 7) e1 <$> exprLevel4)
                  <|> pure e1

exprLevel3 = do
              e1 <- exprLevel2
              (do op <- throughSymbol <|> untilSymbol <|> downtoSymbol <|> downthroughSymbol <|> stepSymbol
                  BinaryExpr (Op op 6) e1 <$> exprLevel3)
                  <|> pure e1

exprLevel2 = do
              e1 <- exprLevel1
              (do op <- appendSymbol
                  BinaryExpr (Op op 5) e1 <$> exprLevel2)
                  <|> pure e1

exprLevel1 = do
              e <- exprLevel0
              (do op <- lshiftSymbol <|> rshiftSymbol
                  BinaryExpr (Op op 4) e <$> exprLevel1)
                  <|> pure e

exprLevel0 = do
              t <- term
              (do op <- addSymbol <|> subSymbol <|> infix5
                  BinaryExpr (Op op 3) t <$> exprLevel0)
                  <|> pure t

term = do
        s <- expterm
        (do op <- mulSymbol <|> divSymbol <|> modSymbol <|> infix6
            BinaryExpr (Op op 2) s <$> term)
            <|> pure s

expterm = do
            s <- subterm
            (do op <- caretSymbol
                BinaryExpr (Op op 1) s <$> expterm)
                <|> pure s

subterm = do
            op <- notSymbol <|> addSymbol <|> subSymbol <|> prefix7
            UnaryExpr (Op op 0) <$> factor
            <|> factor

factor = primary

primary = do
            lparen
            exprs <- sepBy1 expr comma
            rparen
            pure $ mkParenthesis exprs
            <|> do PrimaryExpr <$> subPrimary

mkParenthesis [x] = Parenthesis x
mkParenthesis xs = TupleExpr xs

----------------------------
--  Parser for primaries  --
----------------------------

subPrimary = literalPrimary <|> variablePrimary

variablePrimary = VariablePrimary <$> pattern_

literalPrimary = RealPrimary <$> realLiteral
                 <|>
                     IntPrimary <$> intLiteral
                     <|>
                         CharPrimary <$> charLiteral
                         <|>
                             StrPrimary <$> strLiteral
                             <|>
                                 BoolPrimary <$> boolLiteral


----------------------------
--  Parser for patterns   --
----------------------------

pattern_ = wildcardPattern <|> identifierPattern

wildcardPattern = wildcard >> pure WildcardPattern

identifierPattern = identifier >>= \x -> pure . IdentifierPattern . identifierName . tokenType $ x