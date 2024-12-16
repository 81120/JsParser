module BasicParser where

import Control.Applicative (Alternative (..))
import Data.Char (
  digitToInt,
  isDigit,
  isSpace,
 )
import Data.Functor (($>))
import Data.List (
  isPrefixOf,
  notElem,
 )

newtype Parser a = Parser
  { runParser :: String -> Maybe (a, String)
  }

zero :: Parser a
zero = Parser $ const Nothing

identity :: a -> Parser a
identity x = Parser $ \s -> Just (x, s)

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = Parser $
    \s -> case runParser p s of
      Nothing -> Nothing
      Just (r, s') -> Just (f r, s')

instance Applicative Parser where
  pure :: a -> Parser a
  pure = identity

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pu <*> pv = Parser $
    \s ->
      case runParser pu s of
        Nothing -> Nothing
        Just (f, s') -> runParser (f <$> pv) s'

instance Monad Parser where
  return :: a -> Parser a
  return = pure

  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = Parser $
    \s -> case runParser p s of
      Nothing -> Nothing
      Just (r, s') -> runParser (f r) s'

instance Alternative Parser where
  empty :: Parser a
  empty = zero

  (<|>) :: Parser a -> Parser a -> Parser a
  pu <|> pv = Parser $
    \s -> case runParser pu s of
      Nothing -> runParser pv s
      r -> r

item :: Parser Char
item = Parser $
  \case
    [] -> Nothing
    (x : xs) -> Just (x, xs)

satisify :: (Char -> Bool) -> Parser Char
satisify f = Parser $
  \case
    [] -> Nothing
    (x : xs) ->
      if f x
        then Just (x, xs)
        else Nothing

char :: Char -> Parser Char
char c = satisify (== c)

string :: String -> Parser String
string str = Parser $
  \s ->
    if str `isPrefixOf` s
      then Just (str, drop (length str) s)
      else Nothing

many0 :: Parser a -> Parser [a]
many0 p = Parser $
  \s -> case runParser p s of
    Nothing -> Just ([], s)
    Just (r, s') ->
      case runParser (many0 p) s' of
        Nothing -> Just ([r], s')
        Just (r', rs) -> Just (r : r', rs)

many1 :: Parser a -> Parser [a]
many1 p = do
  x <- p
  xs <- many0 p
  return (x : xs)

sepBy0 :: Parser a -> Parser b -> Parser [a]
pu `sepBy0` pv = Parser $
  \s -> case runParser pu s of
    Nothing -> Just ([], s)
    Just (r, s') ->
      case runParser pv s' of
        Nothing -> Just ([r], s')
        Just (_, rs) ->
          case runParser (pu `sepBy0` pv) rs of
            Nothing -> Just ([r], rs)
            Just (r', rs') -> Just (r : r', rs')

sepBy1 :: Parser a -> Parser b -> Parser [a]
pu `sepBy1` pv = do
  x <- pu <* pv
  xs <- pu `sepBy0` pv
  return (x : xs)

spaces :: Parser String
spaces = many0 (satisify isSpace)

token :: Parser a -> Parser a
token p = spaces *> p <* spaces

symbol :: String -> Parser String
symbol s = token (string s)

digit :: Parser Int
digit = digitToInt <$> satisify isDigit

sign :: Parser Int
sign =
  (symbol "+" $> 1)
    <|> (symbol "-" $> -1)
    <|> return 1

integer :: Parser Int
integer = do
  s <- sign
  c <- many1 (satisify isDigit)
  return (s * read c)

double :: Parser Double
double = do
  s <- sign
  i <- many1 (satisify isDigit)
  _ <- char '.'
  p <- many1 (satisify isDigit)
  return (fromIntegral s * (read (i <> "." <> p) :: Double))

takeUntil :: Char -> Parser String
takeUntil c = Parser $
  \s ->
    let p = takeWhile (/= c) s
     in Just (p, drop (length p) s)

commonStr :: Parser String
commonStr =
  symbol "\""
    *> takeUntil '\"'
    <* symbol "\""

oneOf :: String -> Parser Char
oneOf str = satisify (`elem` str)