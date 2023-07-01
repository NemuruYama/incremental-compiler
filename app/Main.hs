module Main where

import Language.IncrementalCompiler.Lexer

main :: IO ()
main = do
  let (res, errs) = scan "var" "hello2"
  print errs
  putStrLn "Hello, Haskell!!"
