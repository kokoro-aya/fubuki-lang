module ADT where
import Token ( Token(tokenType) )
import GHC.Show (Show)

data Op = Op { opType :: Token, opPrec :: Int } deriving (Eq, Show)

data Expr = UnaryExpr Op Expr
          | BinaryExpr Op Expr Expr
          | PrimaryExpr Primary
          | Parenthesis Expr
          | TupleExpr [Expr]
          | SwitchExpr Expr [(Maybe Primary, Expr)]
          | ChainedMethodExpr Expr [Primary] deriving (Eq, Show)

data Primary = IntPrimary Int
             | RealPrimary Double
             | CharPrimary Char
             | StrPrimary String
             | BoolPrimary Bool
             | ArrayPrimary [Expr]
             | FunctionCallPrimary String [String] [(Maybe String, Expr)]
             | FunctionDeclarationPrimary Declaration
             | VariablePrimary Pattern deriving (Eq, Show)

data Type = FunctionType [Type] Type
          | ArrayType Type
          | TupleType [Type]
          | SimpleType String deriving (Eq, Show)

data Pattern = WildcardPattern
             | IdentifierPattern String (Maybe Type)
             | TuplePattern [Pattern]
             | SubscriptPattern String [Subscript] (Maybe Type) deriving (Eq, Show)

data Subscript = SimpleSubscript Expr
               | SliceSubscript Expr Expr 
               | FromSubscript Expr 
               | ToSubscript Expr deriving (Eq, Show)

data Statement = DeclStatement Declaration
               | ExprStatement Expr
               | AssignmentStatement Op Pattern Expr
               | ForInStatement Pattern Expr CodeBlock
               | WhileStatement [Expr] CodeBlock
               | RepeatWhileStatement [Expr] CodeBlock 
               | IfStatement IfBranch
               | SwitchStatement Expr [SwitchCase]
               | BreakStatement | ContinueStatement | FallthroughStatement | ReturnStatement (Maybe Expr)
               | DoStatement CodeBlock deriving (Eq, Show)

data IfBranch = IfBranch [Expr] CodeBlock 
              | ElseBranch CodeBlock 
              | IfElseBranch [Expr] CodeBlock IfBranch deriving (Eq, Show)

data SwitchCase = SwitchCase [Primary] CodeBlock | DefaultCase CodeBlock deriving (Eq, Show)

type CodeBlock = [Statement]

data Declaration = ValDecl [PatternInitializer]
                 | VarDecl [PatternInitializer]
                 | FuncDecl (Maybe FuncName) [String] [Param] (Maybe Type) FuncBody deriving (Eq, Show)

data FuncName = FuncName String | OperName String deriving (Eq, Show)

data PatternInitializer = SimpleInitializer String (Maybe Type) Expr
                        | DestructInitializer Pattern Expr deriving (Eq, Show)

data Param = ParamName {
    defaultName :: Maybe Name,
    parameterName :: Name,
    paramType :: Type,
    defaultArgument :: Maybe Expr
} deriving (Eq, Show)

data FuncBody = FuncBody CodeBlock | OneLineFuncBody Expr deriving (Eq, Show)

data Name = Name String | Wildcard deriving (Eq, Show)