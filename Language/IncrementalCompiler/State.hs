module Language.IncrementalCompiler.State where

import Language.IncrementalCompiler.Position

data State s e = State
  {
    lexerState :: LexerState e,
    -- parserState :: ParserState s e,
    errors :: [e]
  } deriving (Eq, Show)

-- | The state of the incremental compiler.
data LexerState e = LexerState
  {
    lexerInput :: String,
    lexerOffset :: {-# UNPACK #-} !Int,
    lexerPos :: Position,
    lexerErrors :: [e]
  } deriving (Eq, Show)