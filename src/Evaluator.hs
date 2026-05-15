module Evaluator
  ( EvalError(..), evalEnv, prettyEvalError ) where

import Ast

data EvalError
  = UnknownVariable Name
  | CycleDetected [Name]
  | DuplicateVariable Name
  deriving (Eq, Show)

prettyEvalError :: EvalError -> String
prettyEvalError (UnknownVariable n) = "unknown variable: $" ++ n
prettyEvalError (CycleDetected ns) = "cycle detected: " ++ joinWith " -> " ns
prettyEvalError (DuplicateVariable n) = "duplicate variable: " ++ n

type Defs = [(Name, Value)]
type Env = [(Name, String)]

evalEnv :: EnvFile -> Either EvalError Env
evalEnv (EnvFile bs) =
  case buildDefs bs of
    Left err -> Left err
    Right defs -> evalAll defs (map fst defs) []

buildDefs :: [Binding] -> Either EvalError Defs
buildDefs [] = Right []
buildDefs (Binding name val : rest) =
  case buildDefs rest of
    Left err -> Left err
    Right defs ->
      case lookup name defs of
        Just _ -> Left (DuplicateVariable name)
        Nothing -> Right ((name, val) : defs)

evalAll :: Defs -> [Name] -> Env -> Either EvalError Env
evalAll _ [] cache = Right cache
evalAll defs (n : names) cache =
  case evalVar defs cache [] n of
    Left err -> Left err
    Right (_, cache') -> evalAll defs names cache'

-- path — текущий путь рекурсии, для поиска циклов
evalVar :: Defs -> Env -> [Name] -> Name -> Either EvalError (String, Env)
evalVar defs cache path name =
  case lookup name cache of
    Just v -> Right (v, cache)
    Nothing ->
      if name `elem` path
        then Left (CycleDetected (extractCycle path name))
        else case lookup name defs of
          Nothing -> Left (UnknownVariable name)
          Just ast ->
            case evalValue defs cache (path ++ [name]) ast of
              Left err -> Left err
              Right (v, cache') -> Right (v, (name, v) : cache')

evalValue :: Defs -> Env -> [Name] -> Value -> Either EvalError (String, Env)
evalValue defs cache path val =
  case val of
    Raw cs -> evalChunks defs cache path cs
    SingleQuoted s -> Right (s, cache)
    DoubleQuoted cs -> evalChunks defs cache path cs

evalChunks :: Defs -> Env -> [Name] -> [Chunk] -> Either EvalError (String, Env)
evalChunks _ cache _ [] = Right ("", cache)
evalChunks defs cache path (c : cs) =
  case c of
    TextChunk s ->
      case evalChunks defs cache path cs of
        Left err -> Left err
        Right (rest, cache') -> Right (s ++ rest, cache')
    VarRef n ->
      case evalVar defs cache path n of
        Left err -> Left err
        Right (v, cache') ->
          case evalChunks defs cache' path cs of
            Left err -> Left err
            Right (rest, cache'') -> Right (v ++ rest, cache'')

-- path=["A","B","C"], name="A"  →  ["A","B","C"]
extractCycle :: [Name] -> Name -> [Name]
extractCycle path name =
  dropWhile (/= name) path

joinWith :: String -> [String] -> String
joinWith _ [] = ""
joinWith _ [x] = x
joinWith sep (x : xs) = x ++ sep ++ joinWith sep xs
