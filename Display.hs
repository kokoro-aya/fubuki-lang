module Display where
import ADT
import Token
import Data.List (intercalate)

class Display a where
    display :: a -> String

instance (Display a, Display b) => Display (Either a b) where
    display (Left a) = display a
    display (Right b) = display b

instance (Display a, Display b) => Display (a, b) where
    display (a, b) = "(" ++ display a ++ ", " ++ display b ++ ")"

instance Display a => Display [a] where
    display a = "[" ++ (intercalate ", " . map display $ a) ++ "]"

instance Display Char where
    display c = [c]

instance Display Token where
    display (Token t r c) = display t ++ " at: R:" ++ show r ++ ",C:" ++ show c ++ ".\n"

instance Display TokenType where
    display (Str t) = "\"" ++ t ++ "\""
    display (Chr t) = "'" ++ [t] ++ "'"
    display (Numeric t) = t
    display (Oper t) = t
    display LINTERP = "("
    display RINTERP = ")"
    display (Ident t) = t
    display TRU = "true"
    display FLS = "false"
    display GENERIC_LEFT = "<"
    display GENERIC_RIGHT = ">"
    display LPAREN = "("
    display RPAREN = ")"
    display LBRACKET = "["
    display RBRACKET = "]"
    display LBRACE = "{"
    display RBRACE = "}"
    display COMMA = ","
    display SEMI = ";"
    display ASSIGN = "="
    display LAM_ARR = "=>"
    display ARROW = "->"
    display AMP = "&"
    display ADD = "+"
    display SUB = "-"
    display MUL = "*"
    display DIV = "/"
    display MOD = "%"
    display EQU = "=="
    display NEQU = "!="
    display GANGL = ">"
    display LANGL = "<"
    display GEQ = ">="
    display LEQ = "<="
    display AND = "&&"
    display OR = "||"
    display XOR = "^^"
    display NOT = "!"
    display CARET = "^"
    display ADDEQ = "+="
    display SUBEQ = "-="
    display MULEQ = "*="
    display DIVEQ = "/="
    display MODEQ = "%="
    display LSHIFT = "<<"
    display RSHIFT = ">>"
    display THROUGH = "..."
    display UNTIL = "..<"
    display DOWNTO = ">>."
    display DOWNTHROUGH = ">.."
    display STEP = "@"
    display APPEND = "++"
    display DOT = "."
    display COLUMN = ":"
    display DOUBLE_COLUMN = "::"
    display ULINE = "_"
    display _ = error "token not applicable"

instance Display Op where
    display (Op t p) = display (tokenType t)

instance Display Expr where
    display (UnaryExpr op exp) = display op ++ display exp
    display (BinaryExpr op exp1 exp2) = "(" ++ display exp1 ++ " " ++ display op ++ " " ++ display exp2 ++ ")"
    display (PrimaryExpr pr) = display pr
    display (Parenthesis exp) = "(" ++ display exp ++ ")"
    display (TupleExpr exps) = "(" ++ intercalate ", " (map display exps) ++ ")"
    display (AssignedExpr op patt exp) = display patt ++ " " ++ display op ++ " " ++ display exp

instance Display Primary where
    display (IntPrimary int) = show int
    display (RealPrimary real) = show real
    display (CharPrimary char) = show char
    display (StrPrimary str) = show str
    display (BoolPrimary bool) = show bool
    display (VariablePrimary var) = display var

instance Display Pattern where
    display WildcardPattern = "_"
    display (IdentifierPattern str Nothing) = str
    display (IdentifierPattern str (Just ty)) = str ++ ":" ++ display ty
    display (TuplePattern pats) = "(" ++ intercalate ", " (map display pats) ++ ")"
    
instance Display Type where
    display (FunctionType tx ty) = "(" ++ intercalate "," (map display tx) ++ " -> " ++ display ty ++ ")"
    display (ArrayType ty) = "[" ++ display ty ++ "]"
    display (TupleType ty) = "(" ++ intercalate ", " (map display ty) ++ ")"
    display (SimpleType ty) = ty