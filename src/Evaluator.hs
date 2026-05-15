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
    Right _ -> Right []

buildDefs :: [Binding] -> Either EvalError Defs
buildDefs [] = Right []
buildDefs (Binding name val : rest) =
  case buildDefs rest of
    Left err -> Left err
    Right defs ->
      case lookup name defs of
        Just _ -> Left (DuplicateVariable name)
        Nothing -> Right ((name, val) : defs)

joinWith :: String -> [String] -> String
joinWith _ [] = ""
joinWith _ [x] = x
joinWith sep (x : xs) = x ++ sep ++ joinWith sep xs
