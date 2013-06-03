import Data.List (intercalate)
import System.Environment (getArgs)

import Language.Py.AST (ModuleSpan)
import Language.Py.SrcLocation (SrcSpan(..))
import Language.Py.Parser (parseModule)

import Text.JSON (encode)
import Text.JSON.Generic (toJSON)

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

scanFile :: Float -> String -> IO ()
scanFile minmass file = do
  content <- getFixedContent file
  case parseModule content file of
    Left e -> return ()
    Right (m, _) -> case messages m of
      [] -> return ()
      ms -> putStrLn $ "json+complexity:" ++ encode (toJSON (file, ms))
  where
    messages = map msgToRaw . filter isComplex . moduleMsgs
    isComplex = (> minmass) . (\(_, mass, _) -> mass)

msgToRaw :: Msg -> (Float, String, Int, Int)
msgToRaw (path, mass, span) = (mass, name, start, end)
  where
    name = intercalate "." (reverse path)
    start = case span of
      SpanCoLinear {} -> spanRow span
      SpanMultiLine {} -> spanStartRow span
      SpanPoint {} -> spanRow span
      SpanEmpty -> 0
    end = case span of
      SpanCoLinear {} -> spanRow span
      SpanMultiLine {} -> spanEndRow span
      SpanPoint {} -> spanRow span
      SpanEmpty -> 0

main = do
  minmass <- fmap head getArgs
  files <- fmap tail getArgs
  mapM_ (scanFile (read minmass)) files
