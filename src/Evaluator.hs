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

evalEnv :: EnvFile -> Either EvalError [(Name, String)]
evalEnv = error "TODO: завтра"

joinWith :: String -> [String] -> String
joinWith _ [] = ""
joinWith _ [x] = x
joinWith sep (x : xs) = x ++ sep ++ joinWith sep xs
