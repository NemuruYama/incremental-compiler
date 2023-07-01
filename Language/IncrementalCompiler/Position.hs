module Language.IncrementalCompiler.Position (
  Positioned(..),
  Position(..)
) where

data Position = Position
  {
    line :: Int,
    column :: Int,
    filename :: String
  } deriving (Eq, Show)

class Positioned a where
  position :: a -> Position
  posLine :: a -> Int
  posLine = line . position
  posColumn :: a -> Int
  posColumn = column . position
  posFilename :: a -> String
  posFilename = filename . position