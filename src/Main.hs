module Main where

import System.Environment (getArgs)

import Language.Python.Common.AST (ModuleSpan)
import Language.Python.Common.SrcLocation (SrcSpan)
import Language.Python.Univer.Parser (parseModule)

import Complexity.Massive
import Complexity.MassiveAST

funcs :: ModuleSpan -> [(String, Float, SrcSpan)]
funcs m = foldl go [] (mass 1 m)
  where
    go acc (Func name masses span) = (name, calc masses, span) : acc
    go acc (Simple _) = acc

calc :: [Mass] -> Float
calc masses = foldl go 0 masses
  where
    go acc (Func _ masses _) = acc + calc masses
    go acc (Simple x) = acc + x

main = do
  path   <- fmap head getArgs
  source <- readFile path
  case parseModule source path of
    Left e -> error (path ++ ": " ++ show e)
    Right (m, _) -> print (funcs m)
