module Language.IncrementalCompiler.Position (
  Positioned(..),
  Position(..),
  PositionPoint(..),
  createRange
) where
import Data.List (sortBy)
import Data.Ord (comparing)

data PositionPoint = PositionPoint
  {
    line :: Int,
    column :: Int
  } deriving (Eq, Show)

data Position = Position
  {
    startPos :: PositionPoint,
    endPos :: PositionPoint,
    filename :: String
  } deriving (Eq, Show)

class Positioned a where
  position :: a -> Position
  posLine :: a -> Int
  posLine = line . startPos . position
  posColumn :: a -> Int
  posColumn = column . startPos . position
  posFilename :: a -> String
  posFilename = filename . position

instance Positioned Position where
  position = id

instance Positioned PositionPoint where
  position p = Position p p ""

createRange :: [PositionPoint] -> String -> Position
createRange [] _ = error "Cannot create range from empty list"
createRange [p] fileName = Position p p fileName
createRange ps fileName = Position (head sorted) (last sorted) fileName
  where
    sorted = sortBy (comparing line <> comparing column) ps
