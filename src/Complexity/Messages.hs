module Complexity.Messages
  ( Msg
  , moduleMsgs
  ) where

import Language.Py.AST (ModuleSpan)
import Language.Py.SrcLocation (SrcSpan(..))

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
    go acc (Simple _ _) = acc

calc :: [Mass] -> Float
calc masses = total $ foldl go (0, 0, 0) masses
  where
    total (b, a, o) = sqrt $ sum $ map (^2) [b, a, o]
    go acc (Cls _ masses _) = foldl go acc masses
    go acc (Func _ masses _) = foldl go acc masses
    go (b, a, o) (Simple x Branch) = (b + x, a, o)
    go (b, a, o) (Simple x Assignment) = (b, a + x, o)
    go (b, a, o) (Simple x Other) = (b, a, o + x)
