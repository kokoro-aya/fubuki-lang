{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Parser where

import Token ( Token (line, pos, tokenType), TokenType (Numeric) )
import Control.Applicative ( Alternative(empty, (<|>)) )
import Data.Char (isDigit)
import Display (display)
import Control.Monad.Except (ExceptT, MonadError (throwError, catchError), runExceptT)
import Control.Monad.State
    ( MonadState(put, get), MonadTrans(lift), runState, State )

type Pos = (Int, Int)

data ParseError = DefaultError String | EOFError | UnconsumedError [Token] | MessageError String Pos Token

instance Show ParseError where
    show (DefaultError s) = "Error: " ++ s ++ "."
    show EOFError = "Error: Unexpected end of input."
    show (UnconsumedError t) = "Expected end of input, but there are unconsumed tokens " ++ display t ++ "."
    show (MessageError s p t) = "Error around: " ++ (show . fst $ p) ++ ":" ++ (show . snd $ p) ++ ", " ++ s ++ ", but has " ++ display t ++ "." 

newtype Parser a = P { runParse :: ExceptT ParseError (State [Token]) a }
    deriving (Functor, Applicative, Monad, MonadError ParseError)

parse :: Parser a -> [Token] -> (Either ParseError a, [Token])
parse p tok =
    case (runState . runExceptT . runParse) p tok of
        (Left err, _) -> (Left err, tok)
        (Right x, tok') -> (Right x, tok')

instance Alternative Parser where
    empty = throwError (DefaultError "something happened in the parser")
    p1 <|> p2 = p1 `catchError` const p2

eof :: Parser ()
eof = do x <- P $ lift get
         case x of
            [] -> return ()
            t -> throwError . UnconsumedError $ if length t > 5 then take 5 t else t

satisfy :: String -> (Token -> Bool) -> Parser Token
        -- Error message
satisfy m f = do x <- P $ lift get
                 case x of 
                     [] -> throwError EOFError
                     (t:ts) -> if f t then do P $ lift $ put ts
                                              return t
                                 else throwError (MessageError m (line t, pos t) t)

try :: Parser a -> Parser a
try p = do s <- P $ lift get
           p `catchError` \e -> P $ lift (put s) >> throwError e


choice :: [Parser a] -> Parser a
choice = foldr (<|>) empty

between :: Parser a -> Parser b -> Parser c -> Parser c
between p1 p2 p3 = do _ <- p1
                      x <- p3
                      _ <- p2
                      pure x

--  a | a a | a a a ...

some :: Parser a -> Parser [a]
some p = do x <- p
            xs <- many p
            pure (x:xs)

--  | a | a a | a a a ...
many :: Parser a -> Parser [a]
many p = some p <|> pure []

--  | a b a | a b a b a

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = sepBy1 p sep <|> pure []

--  a | a b a

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = do x <- p
                  xs <- many (sep >> p)
                  pure (x:xs)

sepEndBy :: Parser a -> Parser b -> Parser [a]
sepEndBy p sep = do xs <- sepBy p sep
                    (do _ <- sep
                        pure xs) <|> pure xs

sepEndBy1 :: Parser a -> Parser b -> Parser [a]
sepEndBy1 p sep = do xs <- sepBy1 p sep
                     (do _ <- sep
                         pure xs) <|> pure xs

-- To be used in left associative rules

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) <|> pure a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do x <- p
                  rest x
        where rest a = (do f <- op
                           b <- p
                           rest (f a b))
                       <|> pure a

chainr :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainr p op a = (p `chainr1` op) <|> pure a

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = scan
         where scan = do x <- p
                         rest x
               rest a = do f <- op
                           b <- scan
                           rest (f a b)
                       <|> pure a

-- a b a b a | a a b a | a a a

sepByOpt :: Parser a -> Parser b -> Parser [a]
sepByOpt p sep = do x <- p
                    xs <- many ((do _ <- sep
                                    p)
                                   <|> p)
                    pure (x:xs) <|> pure [x]

-- a b | a

endOptional :: Parser a -> Parser b -> Parser a
endOptional p sep = do x <- p
                       (do sep
                           pure x)
                            <|> pure x

option :: Parser a -> Parser (Maybe a)
option p = Just <$> p <|> pure Nothing

pmap :: (a -> b) -> Parser [a] -> Parser [b]
pmap f p = do xs <- p
              pure $ map f xs

orElse :: a -> Parser a -> Parser a
orElse x p = p <|> pure x


pull :: Parser a -> Parser [a]
pull p = do x <- p
            pure [x]

one = pull $ satisfy "expected 1" (\x -> tokenType x == Numeric "1")
two = pull $ satisfy "expected 2" (\x -> tokenType x == Numeric "2")
three = pull $ satisfy "expected 3" (\x -> tokenType x == Numeric "3")
four = pull $ satisfy "expected 4" (\x -> tokenType x == Numeric "4")
five = pull $ satisfy "expected 5" (\x -> tokenType x == Numeric "5")
six = pull $ satisfy "expected 6" (\x -> tokenType x == Numeric "6")
seven = pull $ satisfy "expected 7" (\x -> tokenType x == Numeric "7")
eight = pull $ satisfy "expected 8" (\x -> tokenType x == Numeric "8")
nine = pull $ satisfy "expected 9" (\x -> tokenType x == Numeric "9")

(>->) :: Parser [a] -> Parser [a] -> Parser [a]
(>->) a b = do a1 <- a
               a2 <- b
               pure $ a1 ++ a2

test1 = (do x <- one
            y <- (two >-> three) <|> (three >-> four) <|> (four >-> five >-> six)
            pure $ x ++ y)
       <|> (do x <- two
               y <- (seven >-> eight) <|> (eight >-> nine)
               z <- one
               pure $ x ++ y ++ z)