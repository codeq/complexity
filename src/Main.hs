module Main where

import Control.Monad (mapM_)
import System.Environment (getArgs)

import Language.Python.Common.AST (ModuleSpan)
import Language.Python.Common.SrcLocation (SrcSpan(..))
import Language.Python.Univer.Parser (parseModule)

import Complexity.Massive
import Complexity.MassiveAST

type Msg = (String, Float, SrcSpan)

msgs :: ModuleSpan -> [Msg]
msgs m = foldl go [] (mass 1 m)
  where
    go acc (Func name masses span) = (name, calc masses, span) : acc
    go acc (Simple _) = acc

calc :: [Mass] -> Float
calc masses = foldl go 0 masses
  where
    go acc (Func _ masses _) = acc + calc masses
    go acc (Simple x) = acc + x

msgToString :: Msg -> String
msgToString (name, mass, span) = prefix (show row) (name ++ " (" ++ show mass ++ ")")
  where
    row = case span of
      SpanCoLinear _ row _ _ -> row
      SpanMultiLine _ row _ _ _ -> row
      SpanPoint _ row _ -> row
      SpanEmpty -> 0

prefix :: String -> String -> String
prefix s1 s2 = s1 ++ ": " ++ s2

main = do
  path   <- fmap head getArgs
  source <- readFile path
  case parseModule source path of
    Left e -> error (path ++ ": " ++ show e)
    Right (m, _) -> mapM_ (putStrLn . prefix path . msgToString) (msgs m)
