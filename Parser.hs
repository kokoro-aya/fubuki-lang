{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Parser where

import Token ( Token (line, pos, tokenType) )
import Control.Applicative ( Alternative(empty, (<|>)) )
import Data.Char (isDigit)
import Display (display)
import Control.Monad.Except
import Control.Monad.State
    ( StateT(StateT), MonadState(get, put), State, runState )

type Pos = (Int, Int)

data ParseError = DefaultError String | EOFError | MessageError String Pos

instance Show ParseError where
    show (DefaultError s) = "error: " ++ s ++ "."
    show EOFError = "error: unexpected end of file."
    show (MessageError s p) = "error: " ++ s ++ ", at around position L" 
        ++ (show . fst $ p) ++ ":" ++ (show . snd $ p) ++ "."

newtype Parser a = P {
        runParser :: ExceptT ParseError (State [Token]) a
    } deriving (Functor, Applicative, Monad, MonadError ParseError)

instance Alternative Parser where
    empty = throwError (DefaultError "something happened in the parser")
    p1 <|> p2 = p1 `catchError` const p2

parse :: Parser a -> [Token] -> Either String (a, [Token])
parse p tok = case (runState . runExceptT . runParser) p tok of
    (Left err, _) -> Left $ show err
    (Right val, rest) -> Right (val, rest)

satisfy :: String -> (Token -> Bool) -> Parser Token
        -- Error message
satisfy m f = do x <- P $ lift get
                 case x of 
                     [] -> throwError EOFError
                     (t:ts) -> if f t then do P $ lift $ put ts
                                              return t
                                 else throwError (MessageError (m ++ ", but has " ++ (show . tokenType $ t)) (line t, pos t))

try :: Parser a -> Parser a
try p = do  s <- P $ lift get
            p `catchError` (\e -> P $ lift (put s) >> throwError e)

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

leftAssociate :: Parser a -> Parser [(b, a)] -> (b -> a -> a -> a) -> Parser a
leftAssociate beg p2 tf = do x <- beg
                             xs <- p2
                             if null xs then
                                pure x
                             else
                                let (hb, ha) = head xs in
                                let rev = reverse $ tail xs in
                                    pure $ foldl (\acc (bx, ax) -> tf bx acc ax) (tf hb x ha) rev

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