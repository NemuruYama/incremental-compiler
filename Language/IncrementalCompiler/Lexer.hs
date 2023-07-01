{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Language.IncrementalCompiler.Lexer where

import Language.IncrementalCompiler.Position ()
import Language.IncrementalCompiler.State ( State (State, lexerState), LexerState (..), errors )
import Language.IncrementalCompiler.Token (PositionedToken)
import Language.IncrementalCompiler.Error ( LexerError )

import Control.Monad.Trans.State (StateT (runStateT), runState)
import qualified Control.Monad.State as S (get, put)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Control.Monad.Identity (Identity, (>=>), runIdentity)
import Control.Applicative (Alternative (..))
import Control.Monad.State (MonadState, get, put)
import Control.Monad.Except (MonadError (throwError), catchError, lift)

data RawToken = 
    WhiteSpaces String
  | ErrorToken [LexerError]
  | RawToken PositionedToken

type ScanState = State String LexerError
newtype Lexer a = Lexer {runLexer :: String -> StateT ScanState (ExceptT ([LexerError], ScanState) Identity) a}

instance Functor Lexer where
  fmap f (Lexer l) = Lexer $ \input -> do
    a <- l input
    return (f a)

instance Applicative Lexer where
  pure a = Lexer $ \input -> return a
  Lexer lF <*> Lexer lA = Lexer $ \input -> do
    f <- lF input
    a <- lA input
    return (f a)

instance Monad Lexer where
  return = pure
  Lexer l >>= f = do
    a <- Lexer l
    f a

instance MonadState ScanState Lexer where
  get = S.get
  put state = Lexer $ \input -> S.put state

instance MonadError [LexerError] Lexer where
  throwError err = Lexer $ \input -> do
    st <- get
    lift $ throwE (err, st)
  catchError (Lexer l) f =
    Lexer $ \input -> do
      st <- get
      let res = runIdentity . runExceptT . flip runStateT st $ l input
      case res of
        Left (errs, st') -> do
          put st'
          let Lexer l' = f errs
          l' input
        Right (res, newSt) -> put newSt >> return res

startState :: ScanState
startState = State {
  lexerState = LexerState {
    lexerInput = "",
    lexerOffset = 0,
    lexerPos = error "LexerState.lexerPos: not implemented",
    lexerErrors = []
  },
  errors = []
}

scan :: String -> ([PositionedToken], [LexerError])
scan input = case runIdentity . runExceptT . flip runStateT startState $ runLexer runTokens input of
  Left any -> error $ show any
  Right _ -> undefined

runTokens :: Lexer [RawToken]
runTokens = runTokens' []
  where
    runTokens' :: [RawToken] -> Lexer [RawToken]
    runTokens' acc = do
      undefined