module Display where
import ADT
import Token
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Char (toLower)

class Display a where
    display :: a -> String

instance (Display a, Display b) => Display (Either a b) where
    display (Left a) = display a
    display (Right b) = display b

instance (Display a, Display b) => Display (a, b) where
    display (a, b) = "(" ++ display a ++ ", " ++ display b ++ ")"

instance Display a => Display [a] where
    display a = "[" ++ (intercalate ", " . map display $ a) ++ "]"

instance Display a => Display (Maybe a) where
    display Nothing = "Nothing"
    display (Just a) = "Just " ++ display a

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
    display GRT = ">"
    display LRT = "<"
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
    display SLICE = ".."
    display QMARK = "?"
    display FALLTHRU = "fallthrough"
    display x = map toLower $ show x

instance Display Op where
    display (Op t _) = display (tokenType t)

instance Display Expr where
    display (UnaryExpr op exp) = display op ++ display exp
    display (BinaryExpr op exp1 exp2) = "(" ++ display exp1 ++ " " ++ display op ++ " " ++ display exp2 ++ ")"
    display (PrimaryExpr pr) = display pr
    display (Parenthesis exp) = "(" ++ display exp ++ ")"
    display (TupleExpr exps) = "(" ++ intercalate ", " (map display exps) ++ ")"
    display (SwitchExpr e cases) = display e ++ " switch {\n" ++ intercalate "\n" (map (\(p, e) -> maybe "default" display p ++ " => " ++ display e) cases) ++ "\n}"
    display (ChainedMethodExpr e ps) = display e ++ "." ++ intercalate "." (map display ps)

instance Display Primary where
    display (IntPrimary int) = show int
    display (RealPrimary real) = show real
    display (CharPrimary char) = show char
    display (StrPrimary str) = "\"" ++ str ++ "\""
    display (BoolPrimary bool) = show bool
    display (VariablePrimary var) = display var
    display (ArrayPrimary ax) = "[" ++ intercalate ", " (map display ax) ++ "]"
    display (FunctionCallPrimary f xs) = f ++ "(" ++ intercalate ", " (map (\(l, e) -> maybe "" (++ ": ") l ++ display e) xs) ++ ")"
    display (FunctionDeclarationPrimary d) = display d

instance Display Pattern where
    display WildcardPattern = "_"
    display (IdentifierPattern str Nothing) = str
    display (IdentifierPattern str (Just ty)) = str ++ "(:" ++ display ty ++ ")"
    display (TuplePattern pats) = "(" ++ intercalate ", " (map display pats) ++ ")"
    display (SubscriptPattern name sx Nothing) = name ++ intercalate "" (map display sx)
    display (SubscriptPattern name sx (Just ty)) = name ++ intercalate "" (map display sx) ++ "(:" ++ display ty ++ ")"

instance Display Type where
    display (FunctionType tx ty) = "(" ++ intercalate ", " (map display tx) ++ ")" ++ " -> " ++ display ty
    display (ArrayType ty) = "[" ++ display ty ++ "]"
    display (TupleType ty) = "(" ++ intercalate ", " (map display ty) ++ ")"
    display (SimpleType ty) = ty

instance Display Subscript where
    display (SimpleSubscript e) = "[" ++ display e ++ "]"
    display (SliceSubscript e1 e2) = "[" ++ display e1 ++ ".." ++ display e2 ++ "]"
    display (FromSubscript e) = "[" ++ display e ++ "..]"
    display (ToSubscript e) = "[.." ++ display e ++ "]"

instance Display Statement where
    display (DeclStatement dec) = display dec
    display (ExprStatement exp) = display exp
    display (AssignmentStatement op p ex) = display p ++ " " ++ display op ++ " " ++ display ex
    display (ForInStatement p e cb) = "for " ++ display p ++ " in " ++ display e ++ " {\n" ++ intercalate "\n" (map display cb) ++ "\n}"
    display (WhileStatement cx cb) = "while " ++ intercalate ", " (map display cx) ++ " {\n" ++ intercalate "\n" (map display cb) ++ "\n}"
    display (RepeatWhileStatement cx cb) = "repeat {\n" ++ intercalate "\n" (map display cb) ++ "\n} while " ++ intercalate ", " (map display cx)
    display (IfStatement ifb) = display ifb
    display (SwitchStatement e cases) = "switch " ++ display e ++ "{\n" ++ intercalate "\n" (map display cases) ++ "\n}"
    display BreakStatement = "break"
    display ContinueStatement = "continue"
    display FallthroughStatement = "fallthrough"
    display (ReturnStatement Nothing) = "return"
    display (ReturnStatement (Just x)) = "return " ++ display x
    display (DoStatement cb) = "do {\n" ++ intercalate "\n" (map display cb) ++ "\n}"

instance Display IfBranch where
    display (IfBranch cx cb) = "if " ++ intercalate ", " (map display cx) ++ " {\n" ++ intercalate "\n" (map display cb) ++ "\n} "
    display (ElseBranch cb) = "{\n" ++ intercalate "\n" (map display cb) ++ "\n}"
    display (IfElseBranch cx cb ifb) = "if " ++ intercalate ", " (map display cx) ++ " {\n" ++ intercalate "\n" (map display cb) ++ "\n} else " ++ display ifb

instance Display SwitchCase where
    display (SwitchCase ps cb) = "case " ++ intercalate ", " (map display ps) ++ ": \n" ++ intercalate "\n" (map display cb) ++ "\n"
    display (DefaultCase cb) = "default : \n" ++ intercalate "\n" (map display cb) ++ "\n"

instance Display Declaration where
    display (ValDecl ps) = "val " ++ intercalate ", " (map display ps)
    display (VarDecl ps) = "var " ++ intercalate ", " (map display ps)
    display (FuncDecl nm prs rt fb) =
        "fn " ++ maybe "" display nm
            ++ "(" ++ intercalate ", " (map display prs) ++ ") " ++ maybe " " (\x -> ": " ++ display x ++ " ") rt ++ display fb

instance Display FuncName where
    display (FuncName x) = x
    display (OperName x) = x

instance Display PatternInitializer where
    display (SimpleInitializer n ty e) = n ++ maybe "" (\x -> ": " ++ display x) ty ++ " = " ++ display e
    display (DestructInitializer p e) = display p ++ " = " ++ display e


instance Display Param where
    display (ParamName nx px pt da) =
        unwords . filter (not . null) $ [maybe "" display nx, display px, ": " ++ display pt, maybe "" display da]

instance Display FuncBody where
    display (FuncBody cb) = "{\n" ++ intercalate "\n" (map display cb) ++ "\n}"
    display (OneLineFuncBody e) = "\n=> " ++ display e

instance Display Name where
    display (Name str) = str
    display Wildcard = "_"