import Data.List (intercalate)
import System.Environment (getArgs)

import Language.Py.SrcLocation (SrcSpan(..))
import Language.Py.Parser (parseModule)

import Text.JSON (encode)
import Text.JSON.Generic (toJSON)

import Complexity.Files
import Complexity.Messages

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
