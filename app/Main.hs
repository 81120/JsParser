module Main where

import BasicParser (
  Parser (runParser),
 )
import JsonParser (jMap)
import LispParser (lispVal)

testJsonParser :: IO ()
testJsonParser = do
  putStrLn "Test JsonParser"
  jsonStr <- readFile "test.json"
  putStrLn jsonStr
  case runParser jMap jsonStr of
    Nothing -> putStrLn "Nothing"
    Just (r, _) -> print r

testLispParser :: IO ()
testLispParser = do
  putStrLn "Test LispParser"
  lStr <- readFile "test.scheme"
  putStrLn lStr
  case runParser lispVal lStr of
    Nothing -> putStrLn "Nothing"
    Just (rs, _) -> print rs

main :: IO ()
main = do
  testJsonParser
  putStrLn ""
  testLispParser
