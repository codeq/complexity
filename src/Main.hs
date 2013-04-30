module Main where

import Control.Monad (mapM_)
import Data.List (intercalate)
import System.Environment (getArgs)

import Language.Python.Common.AST (ModuleSpan)
import Language.Python.Common.SrcLocation (SrcSpan(..))
import Language.Python.Univer.Parser (parseModule)

import Complexity.Files
import Complexity.Massive
import Complexity.MassiveAST

type Msg = ([String], Float, SrcSpan)

moduleMsgs :: ModuleSpan -> [Msg]
moduleMsgs m = msgs [] (mass 1 m)

msgs :: [String] -> [Mass] -> [Msg]
msgs px masses = foldl go [] masses
  where
    go acc (Cls name masses span) = msgs (name : px) masses ++ acc
    go acc (Func name masses span) = (name : px, calc masses, span) : acc
    go acc (Simple _) = acc

calc :: [Mass] -> Float
calc masses = foldl go 0 masses
  where
    go acc (Cls _ masses _) = acc + calc masses
    go acc (Func _ masses _) = acc + calc masses
    go acc (Simple x) = acc + x

msgToString :: Msg -> String
msgToString (path, mass, span) = prefix (show row) (name ++ " (" ++ show mass ++ ")")
  where
    name = intercalate "." (reverse path)
    row = case span of
      SpanCoLinear _ row _ _ -> row
      SpanMultiLine _ row _ _ _ -> row
      SpanPoint _ row _ -> row
      SpanEmpty -> 0

prefix :: String -> String -> String
prefix s1 s2 = s1 ++ ": " ++ s2

scanFile :: String -> IO ()
scanFile file = do
  content <- getFixedContent file
  case parseModule content file of
    Left e -> error (file ++ ": " ++ show e)
    Right (m, _) -> mapM_ (putStrLn . prefix file . msgToString) (moduleMsgs m)

main = do
  files <- getArgs
  mapM_ scanFile files
