module ADT where
import Token ( Token(tokenType) )

data Op = Op { opType :: Token, opPrec :: Int } deriving (Eq)

data Expr = UnaryExpr Op Expr
          | BinaryExpr Op Expr Expr
          | PrimaryExpr Primary
          | Parenthesis Expr
          | TupleExpr [Expr]
          | SwitchExpr Expr [(Maybe Primary, Expr)]
          | ChainedMethodExpr Expr [Primary]
          | AssignedExpr Op Pattern Expr deriving (Eq)

data Primary = IntPrimary Int
             | RealPrimary Double
             | CharPrimary Char
             | StrPrimary String
             | BoolPrimary Bool
             | ArrayPrimary [Expr]
             | FunctionCallPrimary String [(Maybe String, Expr)]
             | FunctionDeclarationPrimary Declaration
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

data Statement = DeclStatement Declaration
               | ExprStatement Expr
               | ForInStatement Pattern Expr CodeBlock
               | WhileStatement [Expr] CodeBlock
               | RepeatWhileStatement [Expr] CodeBlock 
               | IfStatement IfBranch
               | SwitchStatement Expr [SwitchCase]
               | BreakStatement | ContinueStatement | FallthroughStatement | ReturnStatement (Maybe Expr)
               | DoStatement CodeBlock deriving (Eq)

data IfBranch = IfBranch [Expr] CodeBlock 
              | ElseBranch CodeBlock 
              | IfElseBranch [Expr] CodeBlock IfBranch deriving (Eq)

data SwitchCase = SwitchCase [Primary] CodeBlock | DefaultCase CodeBlock deriving (Eq)

type CodeBlock = [Statement]

data Declaration = ValDecl [PatternInitializer]
                 | VarDecl [PatternInitializer]
                 | FuncDecl (Maybe String) [String] [Param] (Maybe Type) FuncBody deriving (Eq)

data PatternInitializer = SimpleInitializer String (Maybe Type) Expr
                        | DestructInitializer Pattern Expr deriving (Eq)

data Param = ParamName {
    defaultName :: Maybe Name,
    parameterName :: Name,
    paramType :: Maybe Type,
    defaultArgument :: Maybe Expr
} deriving (Eq)

data FuncBody = FuncBody CodeBlock | OneLineFuncBody Expr deriving (Eq)

data Name = Name String | Wildcard deriving (Eq)