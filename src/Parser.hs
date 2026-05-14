module Parser where

import Ast
import Data.Char (isAlphaNum, isLetter)
import Data.List (isInfixOf)

isNameChar :: Char -> Bool
isNameChar c = isAlphaNum c || c == '_'

parseName :: String ->(String, String)
parseName [] = ("", [])
parseName (c : cs)
  | isLetter c || c == '_' =
      let (more, rest) = span isNameChar cs
      in (c : more, rest)
  | otherwise = ("", c : cs)

stripExport :: String -> String
stripExport s
  | take 6 s == "export" && length s > 6 && s !! 6 == ' ' =
      dropWhile (== ' ') (drop 6 s)
  | otherwise = s

parseSingleQuoted ::String -> Either String Value
parseSingleQuoted s =
  case break (== '\'') s of
    (_, []) -> Left "error: unclosd single quote"
    (content, _) ->
      if "$(" `isInfixOf` content
        then Left "error: $(...)"
        else Right (SingleQuoted content)
