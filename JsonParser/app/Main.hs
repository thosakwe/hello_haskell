module Main where

import System.IO
import Text.Parsec
import Text.Parsec.String

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Double
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Show)

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
parseJsonNumber =
  -- Written by ChatGPT
  -- Broken, and I'm too tired to fix it right now.
  JsonNumber <$> do
    sign <- option '+' (oneOf "+-")
    intPart <- many1 digit
    fracPart <- option "" (char '.' >> many1 digit)
    expPart <- option "" $ do
      e <- oneOf "eE"
      s <- option '+' (oneOf "+-")
      d <- many1 digit
      return $ e : s : d
    let numStr = sign : intPart ++ fracPart ++ expPart
    case reads numStr of
      [(num, "")] -> return num
      _ -> fail $ "Invalid number: " ++ numStr

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
