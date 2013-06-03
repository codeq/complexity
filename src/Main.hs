module Main where

import Control.Monad (mapM_)
import Data.List (intercalate)
import System.Environment (getArgs)

import Language.Py.SrcLocation (SrcSpan(..))
import Language.Py.Parser (parseModule)

import Complexity.Files
import Complexity.Messages

msgToString :: Msg -> String
msgToString (path, mass, span) = prefix (show row) (name ++ " (" ++ show mass ++ ")")
  where
    name = intercalate "." (reverse path)
    row = case span of
      SpanCoLinear {} -> spanRow span
      SpanMultiLine {} -> spanStartRow span
      SpanPoint {} -> spanRow span
      SpanEmpty -> 0

prefix :: String -> String -> String
prefix s1 s2 = s1 ++ ": " ++ s2

scanFile :: Float -> String -> IO ()
scanFile minmass file = do
  content <- getFixedContent file
  case parseModule content file of
    Left e -> error (file ++ ": " ++ show e)
    Right (m, _) -> mapM_ printMsg $ filter isComplex $ moduleMsgs m
  where
    printMsg = putStrLn . prefix file . msgToString
    isComplex = (> minmass) . (\(_, mass, _) -> mass)

main = do
  minmass <- fmap head getArgs
  files <- fmap tail getArgs
  mapM_ (scanFile (read minmass)) files
