module TextTypes
  ( Symbol
  , WordT(..)
  , Punct(..)
  , Token(..)
  , Sentence(..)
  ) where

type Symbol = Char

newtype WordT = WordT String deriving (Eq, Show)

newtype Punct = Punct Char deriving (Eq, Show)

data Token
  = TWord String   
  | TPunct Char    
  deriving (Eq, Show)

newtype Sentence = Sentence [Token] deriving (Eq, Show)
