module Main where

import System.Environment (getArgs)

import Language.Python.Common.AST (ModuleSpan)
import Language.Python.Univer.Parser (parseModule)

import Complexity.Massive
import Complexity.MassiveAST

funcs :: ModuleSpan -> [(String, Float)]
funcs m = foldl go [] (mass 1 m)
  where
    go acc (Func name masses) = (name, calc masses) : acc
    go acc (Simple _) = acc

calc :: [Mass] -> Float
calc masses = foldl go 0 masses
  where
    go acc (Func _ masses) = acc + calc masses
    go acc (Simple x) = acc + x

main = do
  path   <- fmap head getArgs
  source <- readFile path
  case parseModule source path of
    Left e -> error (path ++ ": " ++ show e)
    Right (m, _) -> print (funcs m)
