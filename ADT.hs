module ADT where
import Token ( Token(tokenType) )

data Op = Op { opType :: Token, opPrec :: Int } deriving (Eq)

data Expr = UnaryExpr Op Expr
          | BinaryExpr Op Expr Expr
          | PrimaryExpr Primary
          | Parenthesis Expr
          | TupleExpr [Expr]
          | AssignedExpr Op Pattern Expr deriving (Eq)

data Primary = IntPrimary Int
             | RealPrimary Double
             | CharPrimary Char
             | StrPrimary String
             | BoolPrimary Bool
             | VariablePrimary Pattern deriving (Eq)

data Pattern = WildcardPattern
             | IdentifierPattern String
             | TuplePattern [Pattern]
             | SubscriptPattern Pattern [Expr] deriving (Eq)