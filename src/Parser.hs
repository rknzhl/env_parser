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
stripExport ('e':'x':'p':'o':'r':'t':' ':rest) = dropWhile (== ' ') rest
stripExport s = s

parseSingleQuoted ::String -> Either String Value
parseSingleQuoted s =
  case break (== '\'') s of
    (_, []) -> Left "error: unclosd single quote"
    (content, _) ->
      if "$(" `isInfixOf` content
        then Left "error: $(...)"
        else Right (SingleQuoted content)

parseRawChunks :: String -> Either String [Chunk]
parseRawChunks [] = Right[]
parseRawChunks ('#' : _) = Right []
parseRawChunks ('$' : c : cs)
  | isLetter c || c == '_' =
      let (more, rest) = span isNameChar cs
      in case parseRawChunks rest of
           Left err -> Left err
           Right chunks -> Right (VarRef (c : more) : chunks)
parseRawChunks (c : cs) =
  case parseRawChunks cs of
    Left err -> Left err
    Right chunks -> Right (TextChunk [c] : chunks)

trimRightChunks :: [Chunk] -> [Chunk]
trimRightChunks [] =[]
trimRightChunks [TextChunk t] =
  let trimmed = reverse (dropWhile  (\c -> c == ' ' || c == '\t') (reverse t))
  in if null trimmed then [] else [TextChunk trimmed]
trimRightChunks (c : cs) = c : trimRightChunks cs

parseDoubleChunks :: String -> Either String [Chunk]
parseDoubleChunks [] = Left "error: unclosed double"
parseDoubleChunks ('"' : _) = Right []
parseDoubleChunks ('$' : c : cs)
  | isLetter c || c == '_'=
      let (more, rest) = span isNameChar cs
      in case parseDoubleChunks rest of
           Left err -> Left err
           Right chunks -> Right (VarRef (c : more) : chunks)
parseDoubleChunks (c : cs) =
  case parseDoubleChunks cs of
    Left err -> Left err
    Right chunks -> Right (TextChunk [c] : chunks)

parseValue :: String -> Either String Value
parseValue [] = Right (Raw [])
parseValue ('"' : rest) =
  case parseDoubleChunks rest of
    Left err -> Left err
    Right chunks -> Right (DoubleQuoted (normalizeChunks chunks))
parseValue ('\'' : rest) = parseSingleQuoted rest
parseValue s =
  case parseRawChunks s of
    Left err -> Left err
    Right chunks -> Right (Raw (normalizeChunks (trimRightChunks chunks)))
