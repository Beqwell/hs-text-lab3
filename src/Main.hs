module Main (main) where

import Processor (normalizeSpaces, splitIntoSentences, swapFirstLastInSentence, renderSentences)

main :: IO ()
main = do
  inp <- readFile "input.txt"      
  let normalized = normalizeSpaces inp
      sentences  = splitIntoSentences normalized
      swapped    = map swapFirstLastInSentence sentences
      out        = renderSentences swapped
  writeFile "output.txt" out
  putStrLn "Result written to output.txt"
