-- Defines token matching rules only, not AST parsing.
module Lexer where

import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)

-- Text.Parsec.Token defines some helper functions that allow us to quickly
-- create parsers for integers, parens, reserved words, etc.
--
-- It's not *necessary*, but super useful.
import qualified Text.Parsec.Token as Tok

-- Helper for creating parsers.
lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    ops = ["+", "*", "-", ";"]
    names = ["def", "extern"]
    style =
      emptyDef
        { Tok.commentLine = "#",
          Tok.reservedOpNames = ops,
          Tok.reservedNames = names
        }

-- | Parse an integer.
integer :: Parser Integer
integer = Tok.integer lexer

-- |  Parse a float (including the .)
float :: Parser Double
float = Tok.float lexer

-- |  Parse something surrounded by ().
parens :: Parser a -> Parser a
parens = Tok.parens lexer

-- |  Parse a list, separated by commas.
commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

-- |  Parse a list, separated by semis.
semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

-- |  Parse an identifier.
identifier :: Parser String
identifier = Tok.identifier lexer

-- |  Parse a reserved word.
reserved :: String -> Parser ()
reserved = Tok.reserved lexer

-- |  Parse a reserved operation.
reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer