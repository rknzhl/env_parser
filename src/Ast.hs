module Ast
  ( Name, EnvFile(..), Binding(..), Value(..), Chunk(..)
  , normalizeChunks
  ) where

type Name = String

newtype EnvFile = EnvFile { envBindings :: [Binding] }
  deriving (Eq, Show)

data Binding = Binding Name Value
  deriving (Eq, Show)

data Value
  = Raw [Chunk]
  | SingleQuoted String
  | DoubleQuoted [Chunk]
  deriving (Eq, Show)

data Chunk
  = TextChunk String
  | VarRef Name
  deriving (Eq, Show)

normalizeChunks :: [Chunk] -> [Chunk]
normalizeChunks [] = []
normalizeChunks (TextChunk "" : rest) = normalizeChunks rest
normalizeChunks (TextChunk a : TextChunk b : rest) =
  normalizeChunks (TextChunk (a ++ b) : rest)
normalizeChunks (x : rest) = x : normalizeChunks rest