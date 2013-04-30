module Complexity.Files (
  getFixedContent
) where

getFixedContent :: String -> IO String
getFixedContent filename = do
  content <- readFile filename
  return $ withNewLine content
  where isNewLine      = (==) '\n'
        addNewLine str = '\n' : str
        withNewLine    = reverse . addNewLine . dropWhile isNewLine . reverse
