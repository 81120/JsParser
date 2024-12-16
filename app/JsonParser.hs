module JsonParser where

import BasicParser (
  Parser,
  commonStr,
  double,
  integer,
  sepBy0,
  symbol,
 )
import Control.Applicative (
  Alternative (..),
 )
import Data.Functor (($>))

data JVal
  = JNull
  | JBool Bool
  | JDouble Double
  | JInt Int
  | JStr String
  | JList [JVal]
  | JMap [JProperty]
  deriving (Show, Eq)

type JProperty = (String, JVal)

jVal :: Parser JVal
jVal =
  jMap
    <|> jList
    <|> jStr
    <|> jDouble
    <|> jInt
    <|> jBool
    <|> jNull

-- deal with null
jNull :: Parser JVal
jNull = symbol "null" $> JNull

-- deal with bool value
true :: Parser JVal
true = symbol "true" $> JBool True

false :: Parser JVal
false = symbol "false" $> JBool False

jBool :: Parser JVal
jBool = true <|> false

-- deal with number
number :: Parser Double
number = double <|> (fromIntegral <$> integer)

jDouble :: Parser JVal
jDouble = JDouble <$> double

jInt :: Parser JVal
jInt = JInt <$> integer

-- deal with string
jStr :: Parser JVal
jStr = JStr <$> commonStr

-- deal with list
jList :: Parser JVal
jList =
  JList
    <$> (symbol "[" *> (jVal `sepBy0` symbol ",") <* symbol "]")

-- deal with map
jProperty :: Parser JProperty
jProperty = do
  name <- commonStr
  _ <- symbol ":"
  val <- jVal
  return (name, val)

jMap :: Parser JVal
jMap =
  JMap
    <$> (symbol "{" *> (jProperty `sepBy0` symbol ",") <* symbol "}")
