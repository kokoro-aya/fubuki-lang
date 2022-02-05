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