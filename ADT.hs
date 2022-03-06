module ADT where
import Token ( Token(tokenType) )

data Op = Op { opType :: Token, opPrec :: Int } deriving (Eq)

instance Show Op where
    show (Op t p) = show (tokenType t) ++ "(" ++ show p ++ ")"

data Expr = UnaryExpr Op Expr
          | BinaryExpr Op Expr Expr
          | PrimaryExpr Primary
          | Parenthesis Expr
          | TupleExpr [Expr]
          | AssignedExpr Op Pattern Expr deriving (Eq, Show)

data Primary = IntPrimary Int
             | RealPrimary Double
             | CharPrimary Char
             | StrPrimary String
             | BoolPrimary Bool
             | VariablePrimary Pattern deriving (Eq, Show)

data Pattern = WildcardPattern
             | IdentifierPattern String
             | TuplePattern [Pattern]
             | SubscriptPattern Pattern [Expr] deriving (Eq, Show)