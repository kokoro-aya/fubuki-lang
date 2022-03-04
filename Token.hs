module Token where

data Token = Token {
    tokenType :: TokenType,
    line :: Int,
    pos :: Int
} deriving (Eq)

instance Show Token where
    show (Token t r c) = show t ++ " at: R:" ++ show r ++ ",C:" ++ show c ++ ".\n"

data TokenType = Numeric String
            | Str String
            | Chr Char
            | Oper String
            | LINTERP | RINTERP
            -- | UChr String -- won't achieve unicode in simple implementation
            | Ident String
            | TRU | FLS
            | GENERIC_LEFT | GENERIC_RIGHT              -- < > 
            | LPAREN | RPAREN                           -- ( )
            | LBRACKET | RBRACKET                       -- [ ]
            | LBRACE | RBRACE                           -- { }
            | COMMA | SEMI                              -- , ;
            | ASSIGN                                    -- =
            
            | LAM_ARR | ARROW                           -- => ->
            | AMP                                       -- &
            | ADD | SUB | MUL | DIV | MOD               -- + - * / %
            | EQU | NEQU | GANGL | LANGL | GEQ | LEQ    -- == != > < >= <=
            | AND | OR | XOR | NOT | CARET          -- && || ^^ ! ^
            | ADDEQ | SUBEQ | MULEQ | DIVEQ | MODEQ     -- += -= *= /= %=
            | LSHIFT | RSHIFT                           -- << >>
            | THROUGH | UNTIL | DOWNTO | DOWNTHROUGH    -- ... ..< >>. >..
            | STEP                                      -- @
            | APPEND                                    -- ++
            | DOT | COLUMN | DOUBLE_COLUMN              -- . : ::

            | ULINE                                 -- _
            | FOR | IN | IF | ELSE | WHILE | RETURN -- for, in, if, else, while, return
            | VAL | VAR | FN                        -- val, var, fn
            | REPEAT | SWITCH | CASE | DEFAULT      -- repeat, switch, case, default
            | BREAK | CONTINUE | FALLTHRU           -- when, break, continue, fallthrough (:||)
            deriving (Eq, Show)

isLiteral :: TokenType -> Bool
isLiteral tok = case tok of
    Str _ -> True ; Chr _ -> True ; Numeric _ -> True
    TRU -> True ; FLS -> True
    _ -> False

isReference :: TokenType -> Bool
isReference (Ident _) = True
isReference _ = False

isOperator :: TokenType -> Bool
isOperator tok = case tok of
    ADD -> True ; SUB -> True ; MUL -> True ; DIV -> True ; MOD -> True
    EQU -> True ; NEQU -> True ; GANGL -> True ; LANGL -> True ; GEQ -> True ; LEQ -> True
    AND -> True ; OR -> True ; XOR -> True ; NOT -> True ; CARET -> True
    ADDEQ -> True ; SUBEQ -> True ; MULEQ -> True ; DIVEQ -> True ; MODEQ -> True
    LSHIFT -> True ; RSHIFT -> True
    THROUGH -> True ; UNTIL -> True ; DOWNTO -> True ; DOWNTHROUGH -> True
    STEP -> True
    APPEND -> True
    (Oper _) -> True
    _ -> False

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