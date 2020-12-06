{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module NanoParsec where

import Data.Char
import Control.Monad
import Control.Applicative

newtype Parser a = Parser { parse :: String -> [(a,String)] }

runParser :: Parser a -> String -> a
runParser m s =
  case parse m s of
    [(res, [])] -> res
    [(_, rs)]   -> error ("Parser did not consume entire stream. " ++ rs)
    _           -> error "Parser error."

runParserMaybe :: Parser a -> String -> Maybe a
runParserMaybe m s =
  case parse m s of
    [(res, [])] -> Just res
    [(_, rs)]   -> Nothing
    _           -> Nothing

item :: Parser Char
item = Parser $ \case
   []     -> []
   (c:cs) -> [(c,cs)]


bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

unit :: a -> Parser a
unit a = Parser (\s -> [(a,s)])

instance Functor Parser where
  fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s])

instance Applicative Parser where
  pure = return
  (Parser cs1) <*> (Parser cs2) = Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])

instance Monad Parser where
  return = unit
  (>>=)  = bind

instance MonadPlus Parser where
  mzero = failure
  mplus = combine

instance Alternative Parser where
  empty = mzero
  (<|>) = option


-- |Parse a or b
combine :: Parser a -> Parser a -> Parser a
combine p q = Parser (\s -> parse p s ++ parse q s)


-- |Parse failure
failure :: Parser a
failure = Parser (const [])


-- |Parse a or b
option :: Parser a -> Parser a -> Parser a
option  p q = Parser $ \s ->
  case parse p s of
    []     -> parse q s
    res    -> res


-- |Parse a character satisfying something
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item `bind` \c ->
  if p c
  then unit c
  else failure

-- | Parse n times something return nothing
parseTimes :: Int -> Parser a -> Parser [a]
parseTimes 0 _ = return []
parseTimes t p = p >>= \x -> (x:) <$> parseTimes (t-1) p


-- |Parse a character from [Char]
oneOf :: String -> Parser Char
oneOf s = satisfy (`elem` s)


chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) <|> return a


-- |Chain parsers p and reduce with op
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do {a <- p; rest a}
  where rest a = (do f <- op
                     b <- p
                     rest (f a b))
                 <|> return a


-- |Parse a specific character
char :: Char -> Parser Char
char c = satisfy (c ==)


-- |Parse multiple instances
multiple :: Parser a -> Parser [a]
multiple x = liftM2 (:) x (multiple x <|> return [])


-- |Parse a specific String
string :: String -> Parser String
string [] = return []
string (c:cs) = do { char c; string cs; return (c:cs)}


-- |Parse some alpha charater sequence
str :: Parser String
str = some (satisfy isAlpha)


strAll :: Parser String
strAll = some (satisfy $ not . (`elem` " \n\r"))


-- |Parse some digit sequence as Integer
natural :: Parser Integer
natural = read <$> some (satisfy isDigit)


-- |Parse something followed by whitespace
token :: Parser a -> Parser a
token p = do { a <- p; spaces ; return a}


-- |Parse a string followed by whitespace
reserved :: String -> Parser String
reserved s = token $ string s


-- |Parse much whitespace
spaces :: Parser String
spaces = many $ oneOf " \n\r"


-- |Parse one digit
digit :: Parser Char
digit = satisfy isDigit


-- |Parse some digit sequence as Int
number :: Parser Int
number = do
  s <- string "-" <|> return []
  cs <- some digit
  return $ read (s ++ cs)


-- |Parse a something inside parentheses
parens :: Parser a -> Parser a
parens m = do
  reserved "("
  n <- m
  reserved ")"
  return n
