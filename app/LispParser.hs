module LispParser where

import BasicParser (
  Parser,
  commonStr,
  double,
  integer,
  many0,
  oneOf,
  satisify,
  sepBy0,
  spaces,
  symbol,
 )
import Control.Applicative (
  Alternative (..),
 )
import Data.Char (
  isDigit,
  isLetter,
  isSymbol,
 )
import Data.Functor (($>))

data LispVal
  = LispInt Int
  | LispDouble Double
  | LispBool Bool
  | LispStr String
  | LispAtom String
  | LispList [LispVal]
  deriving (Eq, Show)

lispVal :: Parser LispVal
lispVal =
  lispList
    <|> lispAtom
    <|> lispStr
    <|> lispDouble
    <|> lispInt
    <|> lispBool

lispInt :: Parser LispVal
lispInt = LispInt <$> integer

lispDouble :: Parser LispVal
lispDouble = LispDouble <$> double

lispBool :: Parser LispVal
lispBool = true <|> false
  where
    true = LispBool <$> (symbol "#t" $> True)
    false = LispBool <$> (symbol "#f" $> False)

lispStr :: Parser LispVal
lispStr = LispStr <$> commonStr

specialChar :: Parser Char
specialChar = oneOf "!@#$%^&*-+_=<>?/,.\\|"

lispAtom :: Parser LispVal
lispAtom = do
  first <-
    satisify isLetter
      <|> satisify isSymbol
      <|> specialChar
  rest <-
    many0
      ( satisify isLetter
          <|> satisify isSymbol
          <|> satisify isDigit
          <|> specialChar
      )
  let
    r = first : rest
   in
    return $ case r of
      "#t" -> LispBool True
      "#f" -> LispBool False
      _ -> LispAtom r

lispList :: Parser LispVal
lispList =
  LispList
    <$> (symbol "(" *> (lispVal `sepBy0` spaces) <* symbol ")")
