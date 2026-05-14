module Parser where

import Ast
import Data.Char (isAlphaNum, isLetter)
import Data.List (isInfixOf)

isNameChar :: Char -> Bool
isNameChar c = isAlphaNum c || c == '_'
