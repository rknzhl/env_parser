module Main where

import Evaluator (evalEnv, prettyEvalError)
import JsonOutput (encodeEnv)
import Parser (parseEnvFile)
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [f] -> run f
    _ -> putStrLn "Usage: safe-env-parser <file.env>" >> exitFailure

run :: FilePath -> IO ()
run path = do
  src <- readFile path
  case parseEnvFile src of
    Left err -> putStrLn ("Parse error: " ++ err) >> exitFailure
    Right ast ->
      case evalEnv ast of
        Left err -> putStrLn ("Error: " ++ prettyEvalError err) >> exitFailure
        Right env -> putStrLn (encodeEnv env)