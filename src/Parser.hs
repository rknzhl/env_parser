module Parser (parseEnvFile) where

import Ast
import Data.Char (isAlphaNum, isLetter)
import Data.List (isInfixOf)

isNameChar :: Char -> Bool
isNameChar c = isAlphaNum c || c == '_'

isDoubleChar :: Char -> Bool
isDoubleChar c = c /= '"' && c /= '$' && c /= '\\' && c /= '`'

isRawChar :: Char -> Bool
isRawChar c = c /= '$' && c /= '#' && c /= '`'

parseEscape :: Char -> Either String String
parseEscape 'n'= Right "\n"
parseEscape 't' = Right "\t"
parseEscape 'r' = Right "\r"
parseEscape '\\' = Right "\\"
parseEscape '"' = Right "\""
parseEscape '$' = Right "$"
parseEscape '`' = Right "`"
parseEscape c = Left ("unknown esc: \\" ++ [c])

parseBracedRef :: String -> (String -> Either String [Chunk]) -> Either String [Chunk]
parseBracedRef s cont =
  case parseName s of
    ("", _) ->
      case cont s of
        Left err -> Left err
        Right chunks -> Right (TextChunk "${" : chunks)
    (name, rest) ->
      let after = case rest of
                    ('}' : r) -> r
                    r -> r
      in case cont after of
           Left err -> Left err
           Right chunks -> Right (VarRef name : chunks)

parseSimpleRef :: String ->(String -> Either String [Chunk]) -> Either String [Chunk]
parseSimpleRef s cont =
  case parseName s of
    ("", _) ->
      case cont s of
        Left err -> Left err
        Right chunks -> Right (TextChunk "$" : chunks)
    (name, rest) ->
      case cont rest of
        Left err ->Left err
        Right chunks-> Right (VarRef name : chunks)

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
parseDoubleChunks [] = Left "error: unclosed double quote"
parseDoubleChunks ('"' : _) = Right []
parseDoubleChunks ('`' : _) = Left "error: backtick is forbidden"
parseDoubleChunks ('$' : '(' : _) = Left "error: $(...) is forbidden"
parseDoubleChunks ('$' : '{' : rest) = parseBracedRef rest parseDoubleChunks
parseDoubleChunks ('$' : rest) = parseSimpleRef rest parseDoubleChunks
parseDoubleChunks ('\\' : []) = Left "error: trailing backslash"
parseDoubleChunks ('\\' : c : rest) =
  case parseEscape c of
    Left err -> Left err
    Right text ->
      case parseDoubleChunks rest of
        Left err -> Left err
        Right chunks -> Right (TextChunk text : chunks)
parseDoubleChunks s =
  let (text, rest) = span isDoubleChar s
  in case parseDoubleChunks rest of
       Left err -> Left err
       Right chunks -> Right (TextChunk text : chunks)

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
    Right chunks -> Right (Raw (trimRightChunks (normalizeChunks chunks)))

parseAssignment :: String -> Either String Binding
parseAssignment s =
  let stripped = stripExport s
  in case parseName stripped of
    ("", _) -> Left ("bad name: " ++ take 20 stripped)
    (name, rest) ->
      case dropWhile isSpaceChar rest of
        ('=' : valueStr) ->
          case parseValue  (dropWhile isSpaceChar valueStr) of
            Left err -> Left err
            Right val -> Right (Binding name val)
        _ -> Left ("missing '=': " ++ name)
  where
    isSpaceChar c = c == ' ' || c == '\t'


parseLine :: String -> Either String (Maybe Binding)
parseLine [] = Right Nothing
parseLine ('#' : _) = Right Nothing
parseLine s = case parseAssignment s of
  Left err -> Left err
  Right binding -> Right (Just binding)

parseLines :: [String] -> Either String [Binding]
parseLines [] = Right []
parseLines (line : rest) =
  case parseLine (dropWhile isSpaceChar line) of
    Left err ->Left err
    Right Nothing -> parseLines rest
    Right (Just binding) ->
      case parseLines rest of
        Left err -> Left err
        Right rest -> Right (binding : rest)
  where
    isSpaceChar c = c== ' ' || c == '\t'


parseEnvFile :: String -> Either String EnvFile
parseEnvFile input =
  case parseLines (lines input) of
    Left err -> Left err
    Right bindings -> Right (EnvFile bindings)