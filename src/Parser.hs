module Parser where

import Ast
import Data.Char (isAlphaNum, isLetter)
import Data.List (isInfixOf)

isNameChar :: Char -> Bool
isNameChar c = isAlphaNum c || c == '_'

parseName :: String -> (String, String)
parseName [] = ("", [])
parseName (c : cs)
  | isLetter c || c == '_' =
      let (more, rest) = span isNameChar cs
      in (c : more, rest)
  | otherwise = ("", c : cs)
