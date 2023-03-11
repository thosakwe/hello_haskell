module Parser where

import Lexer
import Syntax
import Text.Parsec
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok

-- TODO (thosakwe): Understand and document this...
-- binary s f assoc = Ex.Infix (reservedOp s >> return (BinOp f)) assoc
binary s f = Ex.Infix (reservedOp s >> return (BinOp f))

-- TODO (thosakwe): Understand and document this...
table =
  [ [ binary "*" Times Ex.AssocLeft,
      binary "/" Divide Ex.AssocLeft
    ],
    [ binary "+" Plus Ex.AssocLeft,
      binary "-" Minus Ex.AssocLeft
    ]
  ]

int :: Parser Expr
int = do
  -- Composing Float with fromInteger creates an int -> Expr function.
  -- Applying the fmap <$> operator to this allows us to apply this to the
  -- integer input cleanly.
  Float . fromInteger <$> integer

floating :: Parser Expr
floating = Float <$> float

expr :: Parser Expr
expr = Ex.buildExpressionParser table factor

variable :: Parser Expr
variable = Var <$> identifier

function :: Parser Expr
function = do
  reserved "def"
  name <- identifier
  args <- parens $ many variable
  -- Same as body <- expr; return Function name args body
  Function name args <$> expr

extern :: Parser Expr
extern = do
  reserved "extern"
  name <- identifier
  params <- parens $ many variable
  return $ Extern name params

call :: Parser Expr
call = do
  name <- identifier
  args <- parens $ commaSep expr
  return $ Call name args

factor :: Parser Expr
factor =
  try floating
    <|> try int
    <|> try extern
    <|> try function
    <|> try call
    <|> variable
    <|> parens expr

defn :: Parser Expr
defn =
  try extern
    <|> try function
    <|> expr

contents :: Parser a -> Parser a
-- TODO (thosakwe): Understand and document this
contents innerParser = do
  Tok.whiteSpace lexer
  result <- innerParser
  eof
  return result

toplevel :: Parser [Expr]
toplevel = many $ do
  def <- defn
  reservedOp ";"
  return def

makeStringParser :: Parser a -> String -> Either ParseError a
makeStringParser parser = parse (contents parser) "<stdin>"

parseExpr :: String -> Either ParseError Expr
-- parseExpr = parse (contents expr) "<stdin>"
parseExpr = makeStringParser expr

parseToplevel :: String -> Either ParseError [Expr]
-- parseToplevel = parse (contents toplevel) "<stdin>"
parseToplevel = makeStringParser toplevel

parseCompilationUnit :: String -> Either ParseError [Expr]
parseCompilationUnit = parseToplevel
