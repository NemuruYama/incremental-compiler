import os

os.system("cabal build lib:IncrementalCompiler")
os.system("cabal build exe:IncrementalCompiler")
