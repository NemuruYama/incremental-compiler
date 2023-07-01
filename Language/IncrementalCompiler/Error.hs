-- {-# LANGUAGE GADTs #-}
module Language.IncrementalCompiler.Error 
  (
    -- Error(..),
    LexerError(..)
  ) where

import Language.IncrementalCompiler.Position
    ( Positioned(position), Position (Position) )

class (Positioned a) => Error a where
  errorText :: a -> String
  errorPos :: a -> Position

instance Error LexerError where -- TODO pretty print errors
  errorText = show
  errorPos = lexerErrorPos

data LexerError = LexerError { lexerErrorPos :: Position, lexerErrorType :: LexerErrorType }
  deriving (Eq, Show)

instance Positioned LexerError where
  position = lexerErrorPos

data LexerErrorType =
    LexerErrorUnexpectedChar Char
  | LexerErrorUnexpectedEOF
  deriving (Eq, Show)
