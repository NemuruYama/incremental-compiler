module Main where

import Language.IncrementalCompiler.Lexer

main :: IO ()
main = do
  let (res, errs) = scan "hello"
  print errs
  putStrLn "Hello, Haskell!!"
