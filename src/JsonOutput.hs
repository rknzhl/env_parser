module JsonOutput (encodeEnv) where

encodeEnv :: [(String, String)] -> String
encodeEnv pairs = "{" ++ joinWith ", " (map encodePair pairs) ++ "}"

encodePair :: (String, String) -> String
encodePair (k, v) = "\"" ++ k ++ "\": \"" ++ v ++ "\""

joinWith :: String -> [String] -> String
joinWith _ [] = ""
joinWith _ [x] = x
joinWith sep (x : xs) = x ++ sep ++ joinWith sep xs