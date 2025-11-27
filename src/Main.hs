module Main (main) where

import Processor
  ( normalizeSpaces
  , splitIntoSentences
  , swapFirstLastInSentence
  , renderSentences
  )

main :: IO ()
main = do
  inp <- readFile "input.txt"

  let normalized = normalizeSpaces inp
      processedLines = map processLine (lines normalized)
      out = unlines processedLines

  writeFile "output.txt" out
  putStrLn "Result written to output.txt"

processLine :: String -> String
processLine line
  | null line = ""  
  | otherwise =
      let sentences = splitIntoSentences line
          swapped   = map swapFirstLastInSentence sentences
      in renderSentences swapped
