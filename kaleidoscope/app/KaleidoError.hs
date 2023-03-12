module KaleidoError where

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as B
import Text.Parsec

data KaleidoError = KaleidoError SourcePos String

instance Show KaleidoError where
  show (KaleidoError pos msg) =
    sourceName pos
      ++ ":"
      ++ show (sourceLine pos)
      ++ ":"
      ++ show (sourceColumn pos)
      ++ ": error: "
      ++ msg
