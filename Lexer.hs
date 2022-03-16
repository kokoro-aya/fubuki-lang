module Lexer where
import Data.Char (isAlpha, isDigit, isOctDigit, isSpace)
import Token
import Data.List (isPrefixOf)

-- TODO: introduce position and other info in Lexer Tokens

tripleFst :: (a, b, c) -> a
tripleFst (x, _, _) = x

isIdentHead :: Char -> Bool
isIdentHead c = isAlpha c || c == '_'

isIdentChar :: Char -> Bool
isIdentChar c = isIdentHead c || isDigit c

isBinaryChar :: Char -> Bool
isBinaryChar c = c `elem` "01_"

isDecHeadChar :: Char -> Bool
isDecHeadChar c = c `elem` "0123456789"

isDecChar :: Char -> Bool
isDecChar c = c `elem` "0123456789_"

isOctChar :: Char -> Bool
isOctChar c = c `elem` "01234567_"

isHexChar :: Char -> Bool
isHexChar c = c `elem` "0123456789abcdefABCDEF_"

isSymbolHead :: Char -> Bool
isSymbolHead c = c `elem` "$=-&+-*/%<>~!|^.@:"

isSymbolChar :: Char -> Bool
isSymbolChar c = c `elem` "$=-&+-*%<>~!|^.@:?" -- remove / to prevent clash with comments

dotSymbolChar :: Char -> Bool
dotSymbolChar c = c `elem` ".<>" -- prevent from clashes such as [1..-3]

matchUntil :: (String -> Bool) -> String -> (String, String)
matchUntil _ [] = ([], [])
matchUntil f b@(x:xs) = if f b
                 then ([], b)
                 else let (a', b') = matchUntil f xs
                      in (x:a', b')

split :: Char -> String -> [String]
split c xs = case break (==c) xs of 
  (ls, "") -> [ls]
  (ls, x:rs) -> ls : split c rs

lineBegin = 1
posBegin = 1

parseMultilineString :: String -> (String, String)
parseMultilineString = matchUntil (isPrefixOf "\"\"\"")

parseSimpleString :: String -> (String, String)
parseSimpleString "" = ("", "")                                 -- termination, otherwise the parsing will loop
parseSimpleString ('"' : xs) = ([], xs)
parseSimpleString ('\\' : '"' : xs) = ('"' : a, b)
                            where (a, b) = parseSimpleString xs
parseSimpleString ('\\' : '\\' : xs) = ('\\' : a, b)
                            where (a, b) = parseSimpleString xs
parseSimpleString ax@('\\' : x : xs) | isIdentHead x = ("", ax)
parseSimpleString ('\\' : '(' : xs) = ("", "\\(" ++ xs)
parseSimpleString xs = (a ++ a', b')
                        where (a', b') = parseSimpleString b
                              (a, b) = matchUntil (\x -> isPrefixOf "\"" x || isPrefixOf "\\" x) xs

parseComment :: String -> (String, String)
parseComment = matchUntil (isPrefixOf "\n")

parseMultiComment :: String -> (String, String)
parseMultiComment = matchUntil (isPrefixOf "*/")

tokenize :: String -> Int -> [Int] -> Int -> Int -> ([Token], Int, [Int])
                   -- current layout left parenthesis count
                          -- all left parenthesis count, stored in a stack
                                   -- current row number
                                          -- current position number
tokenize [] n m r p = ([], n, m)

tokenize ('/' : '*' : xs) n m r p = tokenize (drop 2 xs') n m rx (x + px + p' + 2)
    where
        (p', x) = if rx == r then (p, 2) else (posBegin, 0)
        (rx, px) = (r + length cmts - 1, length . last $ cmts)
        cmts = split '\n' xx
        (xx, xs') = parseMultiComment xs

tokenize ('/' : '/' : xs) n m r p = (tripleFst (tokenize xs' n m r posBegin), n, m)
    where
        (_, xs') = parseComment xs

tokenize ('(' : xs) n m r p = (Token LPAREN r p : tripleFst (tokenize xs (n + 1) m r (p + 1)), n, m)

tokenize (')' : xs) n m r p | n == head m = let (xx, xxs) = parseSimpleString xs in
                                        (Token RINTERP r p : Token (Str xx) r (p + 1) : tripleFst (tokenize xxs n (tail m) r (p + 2)), n, m)
                      | n > head m = (Token RPAREN r p : tripleFst (tokenize xs (n - 1) m r (p + 1)), n, m)
                      | otherwise = error $ "negative parenthesis balance encountered, at row " ++ show r ++ ", column " ++ show p ++ "."
tokenize ('[' : xs) n m r p = (Token LBRACKET r p : tripleFst (tokenize xs n m r (p + 1)), n, m)
tokenize (']' : xs) n m r p = (Token RBRACKET r p : tripleFst (tokenize xs n m r (p + 1)), n, m)
tokenize ('{' : xs) n m r p = (Token LBRACE r p   : tripleFst (tokenize xs n m r (p + 1)), n, m)
tokenize ('}' : xs) n m r p = (Token RBRACE r p   : tripleFst (tokenize xs n m r (p + 1)), n, m)

tokenize (',' : xs) n m r p = (Token COMMA r p    : tripleFst (tokenize xs n m r (p + 1)), n, m)
tokenize (';' : xs) n m r p = (Token SEMI r p     : tripleFst (tokenize xs n m r (p + 1)), n, m)

tokenize xs@(x:_) n m r p | isDecHeadChar x = (Token (Numeric dec) r p : tripleFst (tokenize xs' n m r (p + length dec)), n, m)
    where
        (dec, xs') = span isDecChar xs

tokenize ('"' :'"' : '"' : xs) n m r p = (Token (Str str) r p : tripleFst (tokenize xs'' n m (r + rows) (rem + posBegin + 3)), n, m)
    where
        (rows, rem) = (length txt - 1, length . last $ txt)
        txt = split '\n' str
        xs'' = drop 3 xs' -- remove trailing """
        (str, xs') = parseMultilineString xs

tokenize ('"' : xs) n m r p = (Token (Str str) r p : tripleFst (tokenize xs' n m r (p + length str + 1)), n, m)
    where
        (str, xs') = parseSimpleString xs

tokenize ('\\' : '(' : xs) n m r p = (Token LINTERP r p : tripleFst (tokenize xs n (n:m) r (p + 2)), n, m)

tokenize ('\\' : x : xs) n m r p | isIdentHead x = (Token LINTERP r p : matchCharacterizedToken (x : t) r p ++ Token RINTERP r (p + length t + 1) : tripleFst (tokenize xs' n m r (p + length t + 1)), n, m)
    where
        (t, xs') = span isIdentChar xs

tokenize ('\'' : x : '\'' : xs) n m r p = (Token (Chr x) r p : tripleFst (tokenize xs n m r (p + 3)), n, m)

tokenize (x : xs) n m r p | x == '\n' = (tripleFst (tokenize xs n m (r + 1) posBegin), n, m)

tokenize (x : xs) n m r p | isSpace x = (tripleFst (tokenize xs n m r (p + 1)), n, m)

tokenize (x : xs) n m r p | isIdentHead x = (matchCharacterizedToken (x : t) r p ++ tripleFst (tokenize xs' n m r (p + length t + 1)), n, m)
    where
        (t, xs') = span (\x -> isIdentChar x || x `elem` "<> ,") xs -- take into account spacing

tokenize (x : xs) n m r p | isSymbolHead x = (Token (matchSymbolToken (x : t)) r p : tripleFst (tokenize xs' n m r (p + length t + 1)), n, m)
    where
        (t, xs') = if x == '.' then span dotSymbolChar xs else span isSymbolChar xs

tokenize xs n m r p = error $ "unrecognized token: " ++ show xs ++ ", at row " ++ show r ++ ", column " ++ show p ++ "." 

matchCharacterizedToken :: String -> Int -> Int -> [Token]
                                  -- Row of beginning of the sequence
                                         -- Position of beginning of the sequence
matchCharacterizedToken "_" r p = [Token ULINE r p]
matchCharacterizedToken "break" r p = [Token BREAK r p]
matchCharacterizedToken "case" r p = [Token CASE r p]
matchCharacterizedToken "continue" r p = [Token CONTINUE r p]
matchCharacterizedToken "default" r p = [Token DEFAULT r p]
matchCharacterizedToken "do" r p = [Token DO r p]
matchCharacterizedToken "else" r p = [Token ELSE r p]
matchCharacterizedToken "fallthrough" r p = [Token FALLTHRU r p]
matchCharacterizedToken "false" r p = [Token FLS r p]
matchCharacterizedToken "for" r p = [Token FOR r p]
matchCharacterizedToken "fn" r p = [Token FN r p]
matchCharacterizedToken "in" r p = [Token IN r p]
matchCharacterizedToken "if" r p = [Token IF r p]
matchCharacterizedToken "return" r p = [Token RETURN r p]
matchCharacterizedToken "repeat" r p = [Token REPEAT r p]
matchCharacterizedToken "switch" r p = [Token SWITCH r p]
matchCharacterizedToken "true" r p = [Token TRU r p]
matchCharacterizedToken "val" r p = [Token VAL r p]
matchCharacterizedToken "var" r p = [Token VAR r p]
matchCharacterizedToken "while" r p = [Token WHILE r p]
matchCharacterizedToken s r p = Token (Ident name) r p: tokenizeClause clause r (p + length name)
    where (name, clause) = span isIdentChar s
          tokenizeClause "" r p = []
          tokenizeClause (' ':xs) r p = tokenizeClause xs r (p + 1)
          tokenizeClause ('<':xs) r p = Token GENERIC_LEFT r p : tokenizeClause xs r (p + 1)
          tokenizeClause ('>':xs) r p = Token GENERIC_RIGHT r p : tokenizeClause xs r (p + 1)
          tokenizeClause (',':xs) r p = Token COMMA r p : tokenizeClause xs r (p + 1)
          -- need to specify if the first char is an ident char, otherwise it will loop
          tokenizeClause xs@(x:_) r p | isIdentChar x = Token (Ident ax) r p : tokenizeClause bx r (p + length ax)
              where (ax, bx) = span isIdentChar xs
          tokenizeClause (x:_) _ _ = error $ "unexpected character: " ++ show x ++ " in generic clause at row " ++ show r ++ ", column " ++ show p ++ "."
          -- Add additional parameters and change return type to reflect the modifications about generic clauses

matchSymbolToken :: String -> TokenType
matchSymbolToken "=>" = LAM_ARR
matchSymbolToken "->" = ARROW
matchSymbolToken "=" = ASSIGN
matchSymbolToken "&" = AMP
matchSymbolToken "+" = ADD
matchSymbolToken "-" = SUB
matchSymbolToken "*" = MUL
matchSymbolToken "/" = DIV
matchSymbolToken "%" = MOD
matchSymbolToken "==" = EQU
matchSymbolToken "!=" = NEQU
matchSymbolToken "<" = LANGL
matchSymbolToken ">" = GANGL
matchSymbolToken ">=" = GEQ
matchSymbolToken "<=" = LEQ
matchSymbolToken "&&" = AND
matchSymbolToken "||" = OR
matchSymbolToken "^^" = XOR
matchSymbolToken "!" = NOT
matchSymbolToken "^" = CARET
matchSymbolToken "+=" = ADDEQ
matchSymbolToken "-=" = SUBEQ
matchSymbolToken "*=" = MULEQ
matchSymbolToken "/=" = DIVEQ
matchSymbolToken "%=" = MODEQ
matchSymbolToken "<<" = LSHIFT
matchSymbolToken ">>" = RSHIFT
matchSymbolToken "..." = THROUGH
matchSymbolToken "..<" = UNTIL
matchSymbolToken ">>." = DOWNTO
matchSymbolToken ">.." = DOWNTHROUGH
matchSymbolToken "@" = STEP
matchSymbolToken "++" = APPEND
matchSymbolToken ".." = SLICE
matchSymbolToken "." = DOT
matchSymbolToken "::" = DOUBLE_COLUMN
matchSymbolToken ":" = COLUMN
matchSymbolToken "?" = QMARK
matchSymbolToken x = Oper x

lexing :: String -> [Token]
lexing x = tripleFst . tokenize x 0 [0] lineBegin $ posBegin

retrieveToken :: [Token] -> [TokenType]
retrieveToken = map tokenType