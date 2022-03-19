module FubukiParser where

import Token (isLiteral, isReference, isOverridableOperator, Token (tokenType, line, pos, Token), TokenType (Ident, RSHIFT), identifierName, operatorName)
import Parser (satisfy, sepBy1, sepEndBy1, Parser, leftAssociate, sepByOpt, sepBy, some, many, endOptional, option, orElse, sepEndBy)
import ADT
import Fragments
    ( intLiteral, realLiteral, charLiteral, strLiteral, boolLiteral, uline, identifier, lbracket, rbracket, column, arrow, slice, switch, lbrace, lamArr, rbrace, semicolon, lesserthanSymbol, greaterthanSymbol, leqSymbol, geqSymbol, assign, lparen, rparen, comma, for, in_, while, repeat_, if_, else_, default_, case_, break_, continue_, retn, do_, fallthrough, val, var, fn, qmark, dot, backtick, genericLeft, genericRight )
import Control.Applicative ((<|>))
import ParseSymbols (notSymbol, addSymbol, subSymbol, mulSymbol, divSymbol, modSymbol, caretSymbol, lshiftSymbol, appendSymbol, throughSymbol, untilSymbol, downtoSymbol, downthroughSymbol, stepSymbol, eqSymbol, neqSymbol, xorSymbol, andSymbol, orSymbol, addeqSymbol, subeqSymbol, muleqSymbol, diveqSymbol, modeqSymbol, infix0, infix1, infix2, infix3, infix4, infix5, infix6, prefix7)
import Data.Maybe
import ADT (Subscript(SliceSubscript, FromSubscript, ToSubscript), IfBranch (IfBranch))

literalToken = satisfy "literal token expected" (isLiteral . tokenType)
referenceToken = satisfy "reference token expected" (isReference . tokenType)
literalOrReferenceToken = satisfy "literal or reference token expected" ((\x -> isLiteral x || isReference x) . tokenType)
operatorToken = satisfy "literal token expected" (isOverridableOperator . tokenType)

----------------------------
-- Parser for expressions --
----------------------------


expr = do e <- exprLevel9
          (do SwitchExpr e <$> switchExpression)
            <|> pure e

exprLevel9 = exprLevel8


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


exprLevel1_1 = do op <- lshiftSymbol <|> (do a <- greaterthanSymbol 
                                             greaterthanSymbol
                                             let (l, p) = (line a, pos a) in
                                                pure $ Token RSHIFT l p)
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

factor = do p <- primary
            (do cmi <- chainedMethodInvocation
                pure $ ChainedMethodExpr p cmi)
                <|> pure p

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

subPrimary = literalPrimary <|> functionCallPrimary <|> variablePrimary <|> (FunctionDeclarationPrimary <$> functionDeclaration)

variablePrimary = VariablePrimary <$> pattern_

literalPrimary = literal <|> arrayLiteral

arrayLiteral = do
                lbracket
                exprs <- sepEndBy1 expr comma
                rbracket
                pure $ ArrayPrimary exprs

literal = (RealPrimary <$> realLiteral)
                 <|>
                     (IntPrimary <$> intLiteral)
                     <|>
                         (CharPrimary <$> charLiteral)
                         <|>
                             (StrPrimary <$> strLiteral)
                             <|>
                                 (BoolPrimary <$> boolLiteral)

functionCallPrimary = do f <- identifierName . tokenType <$> identifier
                         gs <- option genericClause
                         lparen
                         args <- sepEndBy functionCallArgument comma
                         rparen
                         pure $ FunctionCallPrimary f (fromMaybe [] gs) args

functionCallArgument = (do  i <- identifierName . tokenType <$> identifier
                            do column 
                               e <- expr
                               pure (Just i, e)
                               <|> pure (Nothing, (PrimaryExpr . VariablePrimary) (IdentifierPattern i Nothing)))
                               <|> do e <- expr
                                      pure (Nothing, e)


chainedMethodInvocation = some (dot >> functionCallPrimary)

-----------------------------------------
--  Parser for chained function calls  --
-----------------------------------------

----------------------------
--    Parser for types    --
----------------------------

type_ = do arrayType <|> functionOrTupleType <|> typeIdentifier

arrayType = do lbracket
               t <- type_
               rbracket
               pure $ ArrayType t

functionOrTupleType = do lparen
                         tx <- sepBy type_ comma
                         rparen
                         (do arrow
                             ty <- type_
                             pure $ FunctionType tx ty)
                             <|> pure (TupleType tx)

typeIdentifier = do t <- identifier
                    pure . SimpleType . identifierName . tokenType $ t

typeAnnotation = do column
                    type_

----------------------------
--  Parser for patterns   --
----------------------------

pattern_ = wildcardPattern <|> tuplePattern <|> identifierOrSubscriptPattern -- priority order

wildcardPattern = uline >> pure WildcardPattern

tuplePattern = do lparen
                  ps <- sepBy1 pattern_ comma
                  rparen
                  pure $ TuplePattern ps

identifierOrSubscriptPattern = do id <- identifier
                                  let id_ = identifierName . tokenType $ id in
                                    (do xs <- some (do  lbracket
                                                        s <- subscript
                                                        rbracket
                                                        pure s)
                                        (do SubscriptPattern id_ xs . Just <$> typeAnnotation)
                                            <|> pure (SubscriptPattern id_ xs Nothing))
                                    <|> (do IdentifierPattern id_ . Just <$> typeAnnotation
                                        <|> pure (IdentifierPattern id_ Nothing))

subscript = (do s <- expr
                (do v <- subscriptLatter
                    case v of Nothing -> pure $ FromSubscript s
                              Just v' -> pure $ SliceSubscript s v')
                    <|> pure (SimpleSubscript s))
            <|> do  slice
                    e <- expr
                    pure (ToSubscript e)

subscriptLatter = do slice
                     (do b <- expr
                         pure $ Just b
                         <|> pure Nothing) -- be careful on indentation and parenthesis

----------------------------
--   Switch expressions   --
----------------------------

switchExpression = do switch
                      lbrace
                      arms <- sepByOpt switchExprArm comma
                      rbrace
                      pure arms

switchExprArm = do literalArm <|> defaultArm

literalArm = do l <- literal
                lamArr 
                r <- expr
                pure (Just l, r)

defaultArm = do uline
                lamArr 
                r <- expr
                pure (Nothing, r)

----------------------------
--   Lambda expressions   --
----------------------------

-------------------------------
--   Parser for statements   --
-------------------------------

statements = some statement

statement = do  (do d <- endOptional declaration semicolon
                    pure $ DeclStatement d)
                <|> (do s <- endOptional assignmentStatement semicolon
                        pure s)
                <|> (do e <- endOptional expr semicolon
                        pure $ ExprStatement e)
                <|> loopStatement
                <|> branchStatement
                <|> (do c <- endOptional controlTransferStatement semicolon
                        pure c)
                <|> doStatement

assignmentStatement = do l <- pattern_
                         op <- addeqSymbol <|> subeqSymbol <|> muleqSymbol <|> diveqSymbol <|> modeqSymbol <|> assign
                         r <- expr
                         pure $ AssignmentStatement (Op op 12) l r

loopStatement = forInStatement <|> whileStatement <|> repeatWhileStatement

forInStatement = do for
                    v <- pattern_
                    in_
                    e <- expr
                    s <- codeBlock
                    pure $ ForInStatement v e s

whileStatement = do while
                    e <- sepBy expr comma
                    s <- codeBlock
                    pure $ WhileStatement e s

repeatWhileStatement = do repeat_
                          s <- codeBlock
                          while
                          e <- sepBy1 expr comma
                          pure $ RepeatWhileStatement e s

branchStatement = (IfStatement <$> ifStatement) <|> switchStatement

ifStatement = do if_
                 c <- sepBy1 expr comma
                 s1 <- codeBlock
                 (do e <- elseClause
                     pure $ IfElseBranch c s1 e)
                     <|> pure (IfBranch c s1)

elseClause = do else_
                (do i <- ifStatement
                    pure i
                    <|> (do s <- codeBlock
                            pure $ ElseBranch s))

switchStatement = do switch
                     e <- expr
                     lbrace
                     s <- some switchCase
                     rbrace
                     pure $ SwitchStatement e s

switchCase = (do xs <- caseLabel
                 lbrace
                 st <- statements
                 rbrace
                 pure $ SwitchCase xs st)
                 <|> (do d <- defaultLabel
                         lbrace
                         st <- statements
                         rbrace
                         pure $ DefaultCase st)
                         <|> (do xs <- caseLabel
                                 st <- statements
                                 pure $ SwitchCase xs st)
                                     <|> (do d <- defaultLabel
                                             st <- statements
                                             pure $ DefaultCase st)

caseLabel = do  case_
                xs <-sepBy1 literal comma
                column 
                pure xs

defaultLabel = do default_
                  column
                  pure ()

controlTransferStatement = breakStatement <|> continueStatement <|> returnStatement <|> fallthroughStatement

breakStatement = do break_
                    pure BreakStatement

continueStatement = do continue_
                       pure ContinueStatement

fallthroughStatement = do fallthrough
                          pure FallthroughStatement

returnStatement = do retn
                     (do e <- expr
                         pure $ ReturnStatement (Just e))
                        <|> pure (ReturnStatement Nothing)

doStatement = do do_
                 s <- codeBlock
                 pure $ DoStatement s

codeBlock = do lbrace
               s <- statements
               rbrace
               pure s

---------------------------------
--   Parser for declarations   --
---------------------------------

declaration = constantDeclaration <|> variableDeclaration <|> functionDeclaration

constantDeclaration = do val
                         xs <- sepBy1 patternInitializer comma
                         pure $ ValDecl xs

variableDeclaration = do var
                         xs <- sepBy1 patternInitializer comma
                         pure $ VarDecl xs

patternInitializer = simplePatternInitializer <|> destructPatternInitializer

simplePatternInitializer = do i <- identifier
                              let i_ = identifierName . tokenType $ i in
                                (do t <- typeAnnotation
                                    e <- initializer
                                    pure $ SimpleInitializer i_ (Just t) e)
                                <|> (do assign
                                        e <- expr
                                        pure (SimpleInitializer i_ Nothing e))

destructPatternInitializer = do t <- tuplePattern
                                e <- initializer
                                pure $ DestructInitializer t e

initializer = do assign
                 e <- expr
                 pure e

------------------------------
--   Parser for functions   --
------------------------------

functionDeclaration = do fn
                         fnm <- option funcName
                         gc <- option genericClause
                         sig <- parameterClauses
                         res <- option functionResult
                         b <- functionBody
                         pure $ FuncDecl fnm (fromMaybe [] gc) sig res b

funcName = (do  backtick 
                n <- operatorToken 
                backtick
                pure . OperName . operatorName . tokenType $ n)
            <|> (do  n <- identifier 
                     pure . FuncName . identifierName . tokenType $ n)

genericClause = do  genericLeft 
                    xs <- sepBy identifier comma
                    genericRight
                    pure $ map (identifierName . tokenType) xs


parameterClauses = do lparen
                      ps <- sepBy parameter comma
                      rparen
                      pure ps

parameter = do  (def, par) <- paramName
                typ <- typeAnnotation
                arg <- option defaultArgumentClause
                pure $ ParamName def par typ arg

-- _ x -> argument with default name ; x -> argument without name ; a x -> argument with specified name ; _ -> unnamed argument

paramName = do  i1 <- identifier
                (do i2 <- identifier 
                    pure (Just . Name . identifierName . tokenType $ i1, Name . identifierName . tokenType $ i2)
                    <|> pure (Nothing, Name . identifierName . tokenType $ i1))
            <|> (do uline 
                    (do x <- identifier
                        pure (Just Wildcard, Name . identifierName . tokenType $ x)
                        <|> pure (Nothing, Wildcard)))

defaultArgumentClause = initializer

functionResult = typeAnnotation

functionBody = do lamArr >> expr >>= \e -> pure $ OneLineFuncBody e
                <|> FuncBody <$> codeBlock