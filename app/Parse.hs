{-# LANGUAGE LambdaCase #-}

module Parse where

import Data.Char (isSpace)
import GHC.Base (Alternative (..))

newtype Parser a = Parser (String -> [(a, String)])

zero :: Parser a
zero = Parser (const [])

parse :: Parser a -> String -> [(a, String)]
parse (Parser p) = p

instance Functor Parser where
  fmap g p =
    Parser $
      \cs -> case parse p cs of
        [] -> []
        (a, cs') : _ -> [(g a, cs')]

instance Applicative Parser where
  pure a = Parser $ \cs -> [(a, cs)]
  pg <*> pa =
    Parser $
      \cs -> case parse pg cs of
        [] -> []
        (g, cs') : _ -> parse (fmap g pa) cs'

instance Alternative Parser where
  empty = zero
  p <|> q =
    Parser $
      \cs -> case parse p cs of
        [] -> parse q cs
        (a, cs') : _ -> [(a, cs')]

instance Monad Parser where
  return = pure
  p >>= f =
    Parser $
      \cs ->
        concat
          [ parse (f a) cs' | (a, cs') <- parse p cs
          ]

item :: Parser Char
item = Parser $
  \case
    "" -> []
    (c : cs) -> [(c, cs)]

sat :: (Char -> Bool) -> Parser Char
sat p = do
  c <- item
  if p c
    then
      return c
    else
      zero

char :: Char -> Parser Char
char c = sat (c ==)

string :: String -> Parser String
string [] = return []
string (c : cs) = do
  pc <- char c
  prest <- string cs
  return (pc : prest)

many0 :: Parser a -> Parser [a]
many0 p = many1 p <|> return []

many1 :: Parser a -> Parser [a]
many1 p = do
  a <- p
  as <- many0 p
  return (a : as)

spaces :: Parser String
spaces = many0 (sat isSpace)

token :: Parser a -> Parser a
token p = do
  _ <- spaces
  a <- p
  _ <- spaces
  return a

symbol :: String -> Parser String
symbol symb = token (string symb)

sepBy0 :: Parser a -> Parser b -> Parser [a]
p `sepBy0` sep = p `sepBy1` sep <|> return []

sepBy1 :: Parser a -> Parser b -> Parser [a]
p `sepBy1` sep = do
  a <- p
  as <-
    many0
      ( do
          _ <- sep
          p
      )
  return (a : as)

look :: Parser (Maybe Char)
look =
  Parser $
    \case
      [] -> [(Nothing, [])]
      (c : cs') -> [(Just c, c : cs')]

takeUntil :: Char -> Parser [Char]
takeUntil = consumeRest ""
  where
    consumeRest acc stop = do
      l <- look
      if l == Just stop
        then return []
        else do
          c <- item
          cs <- consumeRest (acc ++ [c]) stop
          return (c : cs)
