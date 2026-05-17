module JsonOutput (encodeEnv) where

encodeEnv :: [(String, String)] -> String
encodeEnv pairs = "{" ++ joinWith ", " (map encodePair pairs) ++ "}"

encodePair :: (String, String) -> String
encodePair (k, v) = "\"" ++ escapeJson k ++ "\": \"" ++ escapeJson v ++ "\""

escapeJson :: String -> String
escapeJson [] = []
escapeJson ('"' : xs) = "\\\"" ++ escapeJson xs
escapeJson ('\\' : xs) = "\\\\" ++ escapeJson xs
escapeJson ('\n' : xs) = "\\n" ++ escapeJson xs
escapeJson ('\t' : xs) = "\\t" ++ escapeJson xs
escapeJson ('\r' : xs) = "\\r" ++ escapeJson xs
escapeJson ('\b' : xs) = "\\b" ++ escapeJson xs
escapeJson ('\f' : xs) = "\\f" ++ escapeJson xs
escapeJson (x : xs) = x : escapeJson xs

joinWith :: String -> [String] -> String
joinWith _ [] = ""
joinWith _ [x] = x
joinWith sep (x : xs) = x ++ sep ++ joinWith sep xs