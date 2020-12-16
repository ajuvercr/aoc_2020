{-# LANGUAGE LambdaCase #-}

module NanoParsec where

import Data.Char
import Data.Functor (($>))
import Control.Monad
import Control.Applicative

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

runParser :: Parser a -> String -> a
runParser m s =
  case parse m s of
    Just (res, "") -> res
    Just (_, x)    -> error ("Parser didn't consume entire stream '" ++ x ++ "'")
    _              -> error "Parse failed"

runParserMaybe :: Parser a -> String -> Maybe a
runParserMaybe m s =
    case parse m s of
    Just (res, "") -> Just res
    _              -> Nothing


item :: Parser Char
item = Parser $ \case
   []     -> empty
   (c:cs) -> pure (c,cs)


unit :: a -> Parser a
unit a = Parser (\s -> pure (a,s))


bind :: Parser a -> (a -> Parser b) -> Parser b
bind x f = Parser $ \s -> case parse x s of
  Nothing -> Nothing
  Just (x, xs) -> parse (f x) xs


instance Functor Parser where
  fmap f p = Parser $ fmap (\(x, s) -> (f x, s)) . parse p
  -- fmap f p = Parser $ parse p >=> \(x, s) -> Just (f x, s)


instance Applicative Parser where
  pure x  = Parser $ \s -> pure (x, s)
  f <*> x = Parser $ \s -> case parse f s of
    Nothing -> Nothing
    Just (f', s') -> case parse x s' of
      Nothing -> Nothing
      Just (x', s'') -> pure (f' x', s'')


instance Monad Parser where
  return = unit
  (>>=)  = bind


instance Alternative Parser where
  empty = Parser (const Nothing)
  (<|>) = option


-- |Parse a or b
option :: Parser a -> Parser a -> Parser a
option  p q = Parser $ \s ->
  case parse p s of
    Nothing     -> parse q s
    res         -> res


-- |Parse a character satisfying something
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \case
  c:cs | p c -> pure (c, cs)
  _          -> empty


-- | Parse n times something return nothing
parseTimes :: Int -> Parser a -> Parser [a]
parseTimes 0 _ = return []
parseTimes t p = (:) <$> p <*> parseTimes (t-1) p


ignore :: Parser a -> Parser ()
ignore p = p $> ()


-- |Parse a character from [Char]
oneOf :: String -> Parser Char
oneOf s = satisfy (`elem` s)


-- |Parse a specific character
char :: Char -> Parser Char
char c = satisfy (c ==)


-- |Parse a specific String
string :: String -> Parser ()
string = foldr ((*>) . char) (return ())


-- |Parse some alpha charater sequence
str :: Parser String
str = plus (satisfy isAlpha)


strAll :: Parser String
strAll = plus (satisfy $ not . (`elem` " \n\r"))


-- match zero or more occurrences
star :: Parser a -> Parser [a]
star p = plus p <|> pure []


-- match one or more occurrences
plus :: Parser a -> Parser [a]
plus p = (:) <$> p <*> star p


-- |Parse something followed by whitespace
token :: Parser a -> Parser a
token p = p <* spaces


-- |Parse a string followed by whitespace
reserved :: String -> Parser ()
reserved s = token $ string s


-- |Parse much whitespace
spaces :: Parser String
spaces = star $ oneOf " \n\r"


-- |Parse one digit
digit :: Parser Char
digit = satisfy isDigit


-- |Parse some digit sequence as UInt
natural :: Parser Integer
natural = read <$> plus digit


-- |Parse some digit sequence as Int
number :: Parser Int
number = read <$> liftM2 (:) (char '-' <|> (char '+' >> return ' ') <|> return ' ') (plus digit)


-- |Parse a something inside parentheses
parens :: Parser a -> Parser a
parens m = reserved "(" *> m <* reserved ")"
