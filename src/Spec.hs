module Main where

import Data.List (sort)
import Evaluator (evalEnv, prettyEvalError)
import Parser (parseEnvFile)
import System.Exit (exitFailure)

main :: IO ()
main = do
  ok "raw" (run "A=1\n") (Right [("A","1")])
  ok "empty val" (run "A=\n") (Right [("A","")])
  ok "spaces" (run "A  =  hello  \n") (Right [("A","hello")])
  ok "comment" (run "# cm\nA=1\n") (Right [("A","1")])
  ok "inline cm" (run "A=1 # cm\n") (Right [("A","1")])
  ok "export" (run "export A=1\n") (Right [("A","1")])

  ok "double interp" (run "N=Ivan\nG=\"Hi, $N\"\n") (Right [("G","Hi, Ivan"),("N","Ivan")])
  ok "braced interp" (run "N=Ivan\nG=\"Hi, ${N}\"\n") (Right [("G","Hi, Ivan"),("N","Ivan")])
  ok "single literal" (run "N=Ivan\nT='Hi $N'\n") (Right [("N","Ivan"),("T","Hi $N")])
  ok "forward ref" (run "G=\"Hi, $N\"\nN=Ivan\n") (Right [("G","Hi, Ivan"),("N","Ivan")])

  ok "esc newline" (run "X=\"a\\nb\"\n") (Right [("X","a\nb")])
  ok "esc dollar" (run "X=\"\\$5\"\n") (Right [("X","$5")])
  ok "esc quote" (run "X=\"say \\\"hi\\\"\"\n") (Right [("X","say \"hi\"")])

  err "$() raw" (run "X=$(w)\n")
  err "$() dq" (run "X=\"$(w)\"\n")
  err "backtick dq" (run "X=\"`w`\"\n")
  err "backtick sq" (run "X='`w`'\n")
  err "bad escape" (run "X=\"\\q\"\n")
  err "no =" (run "NOEQUAL\n")
  err "digit start" (run "1X=1\n")

  err "unknown var" (run "X=\"$MISSING\"\n")
  err "duplicate" (run "X=1\nX=2\n")
  err "cycle" (run "A=\"$B\"\nB=\"$A\"\n")

  putStrLn "\nТесты прошли"

run :: String -> Either String [(String, String)]
run input =
  case parseEnvFile input of
    Left e -> Left e
    Right a ->
      case evalEnv a of
        Left e -> Left (prettyEvalError e)
        Right r -> Right (sort r)

ok :: (Eq a, Show a) => String -> a -> a -> IO ()
ok name got expected
  | got == expected = putStrLn ("ok " ++ name)
  | otherwise = do
      putStrLn ("fail " ++ name)
      putStrLn ("  ожидали: " ++ show expected)
      putStrLn ("  получили: " ++ show got)
      exitFailure

err :: Show a => String -> Either String a -> IO ()
err name result =
  case result of
    Left _ -> putStrLn ("ok " ++ name)
    Right v -> do
      putStrLn ("fail " ++ name ++ " а ждали ошибку: " ++ show v)
      exitFailure