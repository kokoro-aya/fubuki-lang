module ADT where
import Token ( Token(tokenType) )

data Op = Op { opType :: Token, opPrec :: Int } deriving (Eq)

data Expr = UnaryExpr Op Expr
          | BinaryExpr Op Expr Expr
          | PrimaryExpr Primary
          | Parenthesis Expr
          | TupleExpr [Expr]
          | SwitchExpr Expr [(Maybe Primary, Expr)]
          | AssignedExpr Op Pattern Expr deriving (Eq)

data Primary = IntPrimary Int
             | RealPrimary Double
             | CharPrimary Char
             | StrPrimary String
             | BoolPrimary Bool
             | ArrayPrimary [Expr]
             | FunctionCallPrimary String [Expr]
             | VariablePrimary Pattern deriving (Eq)

data Type = FunctionType [Type] Type
          | ArrayType Type
          | TupleType [Type]
          | SimpleType String deriving (Eq)

data Pattern = WildcardPattern
             | IdentifierPattern String (Maybe Type)
             | TuplePattern [Pattern]
             | SubscriptPattern String [Subscript] (Maybe Type) deriving (Eq)

data Subscript = SimpleSubscript Expr
               | SliceSubscript Expr Expr 
               | FromSubscript Expr 
               | ToSubscript Expr deriving (Eq)