module TsParser where

import BasicParser (
  Parser,
  chainl,
  number,
  symbol,
 )
import Control.Applicative (
  Alternative (..),
 )
import Data.Functor (($>))
import Prelude hiding (div)

expr :: Parser Double
expr = term `chainl` addop

term :: Parser Double
term = factor `chainl` mulop

factor :: Parser Double
factor = negativeFactor <|> parensExpr <|> number
  where
    negativeFactor = symbol "-" *> (negate <$> factor)
    parensExpr = symbol "(" *> expr <* symbol ")"

addop :: Parser (Double -> Double -> Double)
addop = add <|> sub
  where
    add = symbol "+" $> (+)
    sub = symbol "-" $> (-)

mulop :: Parser (Double -> Double -> Double)
mulop = mul <|> div
  where
    mul = symbol "*" $> (*)
    div = symbol "/" $> (/)