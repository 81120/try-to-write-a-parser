{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module JsonParser where

import Control.Applicative (Alternative (..))
import Data.Char (digitToInt, isDigit)
import Parse (
  Parser,
  char,
  many0,
  sat,
  sepBy0,
  spaces,
  symbol,
  takeUntil,
 )

data Json
  = JsonNull
  | JsonBool Bool
  | JsonString String
  | JsonInteger Int
  | JsonDouble Double
  | JsonArray [Json]
  | JsonObject [JsonProperty]
  deriving (Eq, Show)

type JsonProperty = (String, Json)

json :: Parser Json
json =
  jsonObject
    <|> jsonArray
    <|> jsonString
    <|> jsonDouble
    <|> jsonInteger
    <|> jsonBool
    <|> jsonNull

jsonNull :: Parser Json
jsonNull = do
  symbol "null"
  return JsonNull

jsonBool :: Parser Json
jsonBool = true <|> false
  where
    true = do
      symbol "true"
      return (JsonBool True)
    false = do
      symbol "false"
      return (JsonBool False)

jsonString :: Parser Json
jsonString = do
  symbol "\""
  str <- takeUntil '\"'
  symbol "\""
  return (JsonString str)

digit :: Parser Int
digit = do
  d <- sat isDigit
  return (digitToInt d)

sign :: Parser Int
sign =
  do
    symbol "+"
    return 1
    <|> do
      symbol "-"
      return (-1)
    <|> do
      return 1

integer :: Parser Int
integer = do
  spaces
  s <- sign
  d <- digitToInt <$> sat isDigit
  if d == 0
    then
      return 0
    else do
      ds <- many0 digit
      return (s * asInt (d : ds))
  where
    asInt ds = sum [d * (10 ^ p) | (d, p) <- zip (reverse ds) [0 ..]]

jsonInteger :: Parser Json
jsonInteger = JsonInteger <$> integer

jsonDouble :: Parser Json
jsonDouble = do
  wholePart <- fromIntegral <$> integer
  char '.'
  fractionalPart <- asFracPt <$> many digit
  return (JsonDouble (wholePart + fractionalPart))
  where
    asFracPt ds = sum [fromIntegral d * (10 ** (-p)) | (d, p) <- zip ds [1 ..]]

jsonArray :: Parser Json
jsonArray = do
  symbol "["
  xs <- json `sepBy0` char ','
  symbol "]"
  return (JsonArray xs)

jsonProperty :: Parser JsonProperty
jsonProperty = do
  name <- (\(JsonString s) -> s) <$> jsonString
  symbol ":"
  value <- json
  return (name, value)

jsonObject :: Parser Json
jsonObject = do
  symbol "{"
  props <- jsonProperty `sepBy0` symbol ","
  symbol "}"
  return (JsonObject props)
