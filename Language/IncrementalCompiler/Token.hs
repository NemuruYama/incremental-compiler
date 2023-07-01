module Language.IncrementalCompiler.Token 
  (
    Byte(..),
    Signed(..),
    PositionedToken(..),
    Token(..)
  ) where

import Language.IncrementalCompiler.Position
  ( 
    Positioned (..), 
    Position 
  )

data Byte = B8 | B16 | B32 | B64
  deriving (Eq)

instance Show Byte where
  show B8 = "8"
  show B16 = "16"
  show B32 = "32"
  show B64 = "64"

data Signed = Signed | Unsigned
  deriving (Eq, Show)

data PositionedToken = PositionedToken Token Position
  deriving (Eq, Show)

instance Positioned PositionedToken where
  position (PositionedToken _ pos) = pos 

-- A token.
data Token = 
    ConstToken
  | VarToken | IntToken Signed Byte | FloatToken | DoubleToken | BoolToken | CharToken
  | BracketSquareOpenToken | BracketSquareCloseToken | BracketCurlyOpenToken | BracketCurlyCloseToken | BracketRoundOpenToken | BracketRoundCloseToken
  | CommaToken | SemicolonToken | ColonToken | DotToken
  | AssignToken | AssignAddToken | AssignSubToken | AssignMulToken | AssignDivToken | AssignModToken | AssignAndToken | AssignOrToken | AssignXorToken | AssignShiftLeftToken | AssignShiftRightToken
  | AddToken | SubToken | MulToken | DivToken | ModToken | ShiftLeftToken | ShiftRightToken | NegToken | NotToken
  | AndToken | OrToken | XorToken
  | EqualToken | NotEqualToken | LessToken | LessEqualToken | GreaterToken | GreaterEqualToken
  | IfToken | ElseToken | WhileToken | ForToken | BreakToken | ContinueToken | ReturnToken | SwitchToken | CaseToken | DefaultToken
  | IdentifierToken String
  | CommentToken String
  | EOFToken
  deriving (Eq, Show)