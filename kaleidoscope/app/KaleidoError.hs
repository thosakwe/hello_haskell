module KaleidoError where

import Text.Parsec

data KaleidoError = KaleidoError SourcePos String deriving (Show)