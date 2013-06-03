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
    go acc (Simple _) = acc

calc :: [Mass] -> Float
calc masses = foldl go 0 masses
  where
    go acc (Cls _ masses _) = acc + calc masses
    go acc (Func _ masses _) = acc + calc masses
    go acc (Simple x) = acc + x
