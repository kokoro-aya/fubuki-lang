module Token where

data Token = Token {
    tokenType :: TokenType,
    line :: Int,
    pos :: Int
} deriving (Eq)

instance Show Token where
    show (Token t _ _) = show t

data TokenType = Numeric String
            | Str String
            | Chr Char
            | Oper String
            | LINTERP | RINTERP
            -- | UChr String -- won't achieve unicode in simple implementation
            | Ident String
            | TRU | FLS
            | LPAREN | RPAREN                           -- ( )
            | LBRACKET | RBRACKET                       -- [ ]
            | LBRACE | RBRACE                           -- { }
            | COMMA | SEMI                              -- , ;
            | ASSIGN                                    -- =
            
            | LAM_ARR | ARROW                           -- => ->
            | AMP                                       -- &
            | ADD | SUB | MUL | DIV | MOD               -- + - * / %
            | EQU | NEQU | GRT | LRT | GEQ | LEQ        -- == != > < >= <=
            | AND | OR | XOR | NOT | CARET              -- && || ^^ ! ^
            | ADDEQ | SUBEQ | MULEQ | DIVEQ | MODEQ     -- += -= *= /= %=
            | LSHIFT | RSHIFT                           -- << >>
            | THROUGH | UNTIL | DOWNTO | DOWNTHROUGH    -- ... ..< >>. >..
            | STEP                                      -- @
            | APPEND                                    -- ++
            | DOT | COLUMN | DOUBLE_COLUMN | SLICE      -- . : :: ..
            | QMARK                                     -- ?

            | ULINE                                 -- _
            | FOR | IN | IF | ELSE | WHILE | RETURN -- for, in, if, else, while, return
            | VAL | VAR | FN                        -- val, var, fn
            | REPEAT | SWITCH | CASE | DEFAULT      -- repeat, switch, case, default
            | DO | BREAK | CONTINUE | FALLTHRU      -- do, break, continue, fallthrough (:||)
            | BACKTICK                              -- `
            deriving (Eq, Show)

isLiteral :: TokenType -> Bool
isLiteral tok = case tok of
    Str _ -> True ; Chr _ -> True ; Numeric _ -> True
    TRU -> True ; FLS -> True
    _ -> False

isReference :: TokenType -> Bool
isReference (Ident _) = True
isReference ULINE = True
isReference _ = False

isOverridableOperator :: TokenType -> Bool
isOverridableOperator tok = case tok of
    ADD -> True ; SUB -> True ; MUL -> True ; DIV -> True ; MOD -> True
    EQU -> True ; NEQU -> True ; GRT -> True ; LRT -> True ; GEQ -> True ; LEQ -> True
    AND -> True ; OR -> True ; XOR -> True ; NOT -> True ; CARET -> True
    LSHIFT -> True ; RSHIFT -> True
    APPEND -> True
    (Oper _) -> True
    _ -> False

matchInfix :: (Char -> Bool) -> TokenType -> Bool
matchInfix p (Oper (c:_)) = p c
matchInfix _ _ = False

isComma :: TokenType -> Bool
isComma COMMA = True
isComma _ = False

literalValue :: TokenType -> String
literalValue (Str s) = s
literalValue (Numeric s) = s
literalValue _ = error "internal:: literalValue: not a literal"

charLiteralValue :: TokenType -> Char
charLiteralValue (Chr c) = c
charLiteralValue _ = error "internal:: charLiteralValue: not a char literal"

identifierName :: TokenType -> String
identifierName (Ident s) = s
identifierName _ = error "internal:: identifierName: not an identifier"

operatorName :: TokenType -> String
operatorName op = case op of
    ADD -> "+" ; SUB -> "-" ; MUL -> "*" ; DIV -> "/" ; MOD -> "%"
    EQU -> "==" ; NEQU -> "!=" ; GRT -> ">" ; LRT -> "<" ; GEQ -> ">=" ; LEQ -> "<="
    AND -> "&&" ; OR -> "||" ; XOR -> "^^" ; NOT -> "!" ; CARET -> "^"
    LSHIFT -> "<<" ; RSHIFT -> ">>"
    APPEND -> "++"
    (Oper x) -> x
    _ -> error "internal:: operatorName: not an operator"