module FubukiParser where

import Token (isLiteral, isReference, isOperator, Token (tokenType), TokenType (Ident), identifierName)
import Parser (satisfy, sepBy1, sepEndBy1, Parser, leftAssociate)
import ADT
import Fragments
    ( intLiteral, realLiteral, charLiteral, strLiteral, boolLiteral, wildcard, identifier )
import Control.Applicative ((<|>))
import ParseSymbols (lparen, comma, rparen, notSymbol, addSymbol, subSymbol, mulSymbol, divSymbol, modSymbol, caretSymbol, lshiftSymbol, rshiftSymbol, appendSymbol, throughSymbol, untilSymbol, downtoSymbol, downthroughSymbol, stepSymbol, lesserthanSymbol, greaterthanSymbol, leqSymbol, geqSymbol, eqSymbol, neqSymbol, xorSymbol, andSymbol, orSymbol, addeqSymbol, subeqSymbol, muleqSymbol, assignSymbol, diveqSymbol, modeqSymbol, infix0, infix1, infix2, infix3, infix4, infix5, infix6, prefix7)
import Data.Maybe

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


exprLevel8 = leftAssociate exprLevel7 exprLevel8_1 BinaryExpr


exprLevel8_1 = do op <- orSymbol <|> infix0
                  ex <- exprLevel7
                  tm1 <- exprLevel8_1
                  pure ((Op op 11, ex):tm1)
                  <|> pure []



exprLevel7 = leftAssociate exprLevel6 exprLevel7_1 BinaryExpr


exprLevel7_1 = do op <- andSymbol <|> infix1
                  ex <- exprLevel6
                  tm1 <- exprLevel7_1
                  pure ((Op op 10, ex):tm1)
                  <|> pure []


exprLevel6 = leftAssociate exprLevel5 exprLevel6_1 BinaryExpr


exprLevel6_1 = do op <- xorSymbol <|> infix2
                  ex <- exprLevel5
                  tm1 <- exprLevel6_1
                  pure ((Op op 9, ex):tm1)
                  <|> pure []


exprLevel5 = leftAssociate exprLevel4 exprLevel5_1 BinaryExpr


exprLevel5_1 = do op <- eqSymbol <|> neqSymbol <|> infix3
                  ex <- exprLevel4
                  tm1 <- exprLevel5_1
                  pure ((Op op 8, ex):tm1)
                  <|> pure []



exprLevel4 = leftAssociate exprLevel3 exprLevel4_1 BinaryExpr


exprLevel4_1 = do op <- lesserthanSymbol <|> greaterthanSymbol <|> leqSymbol <|> geqSymbol <|> infix4
                  ex <- exprLevel3
                  tm1 <- exprLevel4_1
                  pure ((Op op 7, ex):tm1)
                  <|> pure []


exprLevel3 = leftAssociate exprLevel2 exprLevel3_1 BinaryExpr


exprLevel3_1 = do op <- throughSymbol <|> untilSymbol <|> downtoSymbol <|> downthroughSymbol <|> stepSymbol
                  ex <- exprLevel2
                  tm1 <- exprLevel3_1
                  pure ((Op op 6, ex):tm1)
                  <|> pure []


exprLevel2 = leftAssociate exprLevel1 exprLevel2_1 BinaryExpr


exprLevel2_1 = do op <- appendSymbol
                  ex <- exprLevel1
                  tm1 <- exprLevel2_1
                  pure ((Op op 5, ex):tm1)
                  <|> pure []


exprLevel1 = leftAssociate exprLevel0 exprLevel1_1 BinaryExpr


exprLevel1_1 = do op <- lshiftSymbol <|> rshiftSymbol
                  ex <- exprLevel0
                  tm1 <- exprLevel1_1
                  pure ((Op op 4, ex):tm1)
                  <|> pure []


exprLevel0 = leftAssociate term exprLevel0_1 BinaryExpr


exprLevel0_1 = do op <- addSymbol <|> subSymbol <|> infix5
                  ex <- term
                  tm1 <- exprLevel0_1
                  pure ((Op op 3, ex):tm1)
                  <|> pure []


term = leftAssociate expterm term1 BinaryExpr


term1 = do op <- mulSymbol <|> divSymbol <|> modSymbol <|> infix6
           ex <- expterm
           tm1 <- term1
           pure ((Op op 2, ex):tm1)
           <|> pure []

expterm = do -- assoc right
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
            exprs <- sepEndBy1 expr comma
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