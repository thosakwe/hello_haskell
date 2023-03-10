module Main where

import System.IO
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String

import qualified Text.Parsec.Token as Tok

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Double
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Show)

-- Create a lexer for integers, floats
-- () is the state type for the TokenParser, which in this case has no state.
lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser emptyDef

float :: Parser Double
float = Tok.float lexer

integer :: Parser Double
integer = do
  intValue <- Tok.integer lexer
  return $ fromIntegral intValue

parseJsonString :: Parser JsonValue
parseJsonString = do
  between (char '"') (char '"') (JsonString <$> many (noneOf "\""))

parseJsonArray :: Parser JsonValue
parseJsonArray = do
  values <- between (char '[') (char ']') (parseJsonValue `sepBy` char ',')
  return $ JsonArray values

parseJsonPair :: Parser (String, JsonValue)
parseJsonPair = do
  key <- parseJsonString
  char ':'
  value <- parseJsonValue
  case key of
    JsonString key -> return (key, value)
    _ -> error "Expected JsonString"

parseJsonObject :: Parser JsonValue
parseJsonObject = do
  let parseListOfPairs = parseJsonPair `sepBy` char ','
  pairs <- between (char '{') (char '}') parseListOfPairs
  return (JsonObject pairs)

parseJsonBool :: Parser JsonValue
parseJsonBool = do
  boolStr <- string "true" <|> string "false"
  case boolStr of
    "true" -> return $ JsonBool True
    "false" -> return $ JsonBool False
    _ -> error "Boolean must be true or false."

parseJsonNull :: Parser JsonValue
parseJsonNull = string "null" >> return JsonNull

parseJsonNumber :: Parser JsonValue
-- float parse might fail if we entered an integer (as it expects a .), so we
-- wrap float in "try"
parseJsonNumber = JsonNumber <$> (try float <|> integer)

parseJsonValue :: Parser JsonValue
parseJsonValue =
  choice
    [ parseJsonString,
      parseJsonArray,
      parseJsonObject,
      parseJsonBool,
      parseJsonNull,
      parseJsonNumber
    ]

main :: IO ()
main = do
  putStr "Enter a JSON string (without whitespace): "
  hFlush stdout
  input <- getLine
  let jsonEither = parse parseJsonValue "" input
  case jsonEither of
    Left err -> do
      putStrLn "Invalid JSON."
      print err
      main
    Right json -> do
      print json
      main
