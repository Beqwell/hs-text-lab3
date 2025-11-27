module Processor
  ( normalizeSpaces
  , splitIntoSentences
  , swapFirstLastInSentence
  , renderSentences
  ) where

import TextTypes
import Data.Char (isSpace, isAlphaNum)

-- | Collapse tabs 
normalizeSpaces :: String -> String
normalizeSpaces =
  unwords . words . map (\c -> if c == '\t' then ' ' else c)

-- [Token]
tokenize :: String -> [Token]
tokenize = go [] []
  where
    go accWord acc [] =
      let acc' = if null accWord then acc else acc ++ [TWord accWord]
      in acc'
    go accWord acc (c:cs)
      | isAlphaNum c = go (accWord ++ [c]) acc cs
      | c `elem` ".!?" =
          let acc1 = if null accWord then acc else acc ++ [TWord accWord]
          in go [] (acc1 ++ [TPunct c]) cs
      | otherwise =
          let acc1 = if null accWord then acc else acc ++ [TWord accWord]
          in go [] acc1 cs

--  Split by sentence-ending
splitIntoSentences :: String -> [Sentence]
splitIntoSentences s = map (Sentence . tokenize) (cut s)
  where
    cut :: String -> [String]
    cut = go "" []
      where
        go cur acc [] =
          let cur' = trim cur
          in if null cur' then acc else acc ++ [cur']
        go cur acc (c:cs)
          | c `elem` ".!?" =
              let piece = trim (cur ++ [c])
              in go "" (if null piece then acc else acc ++ [piece]) cs
          | otherwise = go (cur ++ [c]) acc cs

    trim = f . f
      where f = reverse . dropWhile isSpace

-- Swap first and last word 
swapFirstLastInSentence :: Sentence -> Sentence
swapFirstLastInSentence (Sentence toks) = Sentence (swap toks)
  where
    indices :: [Int]
    indices = [ i | (i, TWord _) <- zip [0..] toks ]

    swap :: [Token] -> [Token]
    swap ts = case indices of
      []            -> ts                 -- no words
      [_]           -> ts                 -- only one word
      iFirst:rest   ->                    -- at least two words
        let iLast = last rest
        in swapAt iFirst iLast ts

    swapAt i j ts =
      let wi = getWord i ts
          wj = getWord j ts
      in setWord j wi (setWord i wj ts)

    getWord k ts = case ts !! k of
                     TWord w -> w
                     _       -> error "index not a word"

    setWord k w ts = take k ts ++ [TWord w] ++ drop (k+1) ts

renderSentences :: [Sentence] -> String
renderSentences = unwords . map renderOne
  where
    renderOne (Sentence ts) = concat (foldl build [] ts)

    build :: [String] -> Token -> [String]
    build acc (TWord w) =
      case acc of
        [] -> [w]
        _  -> acc ++ [" " ++ w]
    build acc (TPunct p) =
      case acc of
        []         -> [[p]]
        initParts  ->
          let lastPart = last initParts
          in init initParts ++ [lastPart ++ [p]]
