{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.IncrementalCompiler.Lexer where

import Language.IncrementalCompiler.Position (Position (..), PositionPoint (..), createRange)
import Language.IncrementalCompiler.State ( State (State, lexerState), LexerState (..), errors )
import Language.IncrementalCompiler.Token (PositionedToken (..), Token (..))
import Language.IncrementalCompiler.Error ( LexerError (..), LexerErrorType (..) )

import Control.Monad.Trans.State (StateT (runStateT), runState)
import qualified Control.Monad.State as S (get, put)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Control.Monad.Identity (Identity, (>=>), runIdentity)
import Control.Applicative (Alternative (..))
import Control.Monad.State (MonadState, get, put, gets)
import Control.Monad.Except (MonadError (throwError), catchError, lift)

data RawToken =
    WhiteSpaces String
  | RawToken PositionedToken
  deriving (Eq, Show)

type ScanState = LexerState LexerError
newtype Lexer a = Lexer {runLexer :: StateT ScanState (ExceptT ([LexerError], ScanState) Identity) a}
  deriving (Functor, Applicative, Monad, MonadState ScanState, MonadError ([LexerError], ScanState))

instance Alternative Lexer where
  empty = Lexer $ do
    st <- get
    lift $ throwE ([], st)
  Lexer l1 <|> Lexer l2 = Lexer $ do
    l1 `catchError` \(errs, st) -> do
      put st
      l2 `catchError` \(errs', st') -> lift $ throwE (errs, st) -- Always give left priority, maybe either add a flag to change this behavior or make it configurable

startState :: String -> String -> ScanState
startState input fileName = LexerState {
    lexerInput = input,
    lexerOffset = 0,
    lexerPos = PositionPoint {line=1, column=1},
    lexerFileName = fileName,
    lexerErrors = []
  }

getInput :: Lexer String
getInput = gets lexerInput

getOffset :: Lexer Int
getOffset = gets lexerOffset

getPos :: Lexer PositionPoint
getPos = gets lexerPos

getErrors :: Lexer [LexerError]
getErrors = gets lexerErrors

getFileName :: Lexer String
getFileName = gets lexerFileName

putInput :: String -> Lexer ()
putInput input = do
  st <- get
  put st {lexerInput = input}

putOffset :: Int -> Lexer ()
putOffset offset = do
  st <- get
  put st {lexerOffset = offset}

putPos :: PositionPoint -> Lexer ()
putPos pos = do
  st <- get
  put st {lexerPos = pos}

putErrors :: [LexerError] -> Lexer ()
putErrors errs = do
  st <- get
  put st {lexerErrors = errs}

modifyInput :: (String -> String) -> Lexer ()
modifyInput f = do
  st <- get
  put st {lexerInput = f $ lexerInput st}

modifyOffset :: (Int -> Int) -> Lexer ()
modifyOffset f = do
  st <- get
  put st {lexerOffset = f $ lexerOffset st}

modifyPos :: (PositionPoint -> PositionPoint) -> Lexer ()
modifyPos f = do
  st <- get
  put st {lexerPos = f $ lexerPos st}

modifyErrors :: ([LexerError] -> [LexerError]) -> Lexer ()
modifyErrors f = do
  st <- get
  put st {lexerErrors = f $ lexerErrors st}

addError :: LexerError -> Lexer ()
addError err = do
  st <- get
  put st {lexerErrors = err : lexerErrors st}

addErrors :: [LexerError] -> Lexer ()
addErrors errs = do
  st <- get
  put st {lexerErrors = errs ++ lexerErrors st}

createToken :: Token -> Lexer PositionedToken
createToken token = do
  pos <- getPos
  PositionedToken token . createRange [pos] <$> getFileName

createRangeToken :: Token -> PositionPoint -> Lexer PositionedToken
createRangeToken token endPoint = do
  pos <- getPos
  PositionedToken token . createRange [pos, endPoint] <$> getFileName

scan :: String -> String -> ([PositionedToken], [LexerError])
scan input fileName = case runIdentity . runExceptT . flip runStateT (startState input fileName) $ runLexer runTokens of
  Left any -> error $ show any
  Right (res, _) -> rawTokensToPositionedTokens res

rawTokensToPositionedTokens :: [RawToken] -> ([PositionedToken], [LexerError])
rawTokensToPositionedTokens [] = ([], [])
rawTokensToPositionedTokens (RawToken t : ts) = let (ts', errs) = rawTokensToPositionedTokens ts in (t : ts', errs)
rawTokensToPositionedTokens (_ : ts) = rawTokensToPositionedTokens ts

runTokens :: Lexer [RawToken]
runTokens = runTokens' []
  where
    runTokens' :: [RawToken] -> Lexer [RawToken]
    runTokens' acc = do
      eof <- eof

      case eof of
        Just eof' -> return $ reverse $ eof' : acc
        Nothing -> do
          token <- undefined
          runTokens' $ token : acc

eof :: Lexer (Maybe RawToken)
eof = do
  input <- getInput
  if null input
    then Just . RawToken <$> createToken EOFToken
    else return Nothing