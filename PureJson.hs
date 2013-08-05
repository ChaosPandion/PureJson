module PureJson (parseJson) where

import Data.List
import Data.Int 
import qualified Data.Map as Map
import Data.Char
import Data.Maybe
import System.IO
import Text.Printf(printf)
import System.Environment(getArgs)
import System.Exit
import Control.Monad(when)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Prim
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator

data JsonValue =
    JsonNumber Double
    | JsonString String
    | JsonBoolean Bool
    | JsonNull
    | JsonObject (Map.Map String JsonValue)
    | JsonArray [JsonValue]
    deriving (Eq, Ord, Show)
    
parseJson :: String -> Either ParseError JsonValue
parseJson input = parse parseJsonText "" input

parseJsonText :: GenParser Char st JsonValue
parseJsonText = do
    spaces
    value <- parseJsonValue
    spaces
    eof
    return value

parseJsonValue :: GenParser Char st JsonValue
parseJsonValue = do
    spaces
    result <- parseJsonArray <|> parseJsonObject <|> parseJsonNumber <|> parseJsonString <|> parseJsonBoolean <|> parseJsonNull
    spaces
    return result
    
    
parseCommaSep :: GenParser Char st Char
parseCommaSep = do
    spaces
    char ','
    spaces
    return ','
    
parseJsonObjectMember :: GenParser Char st (String, JsonValue)
parseJsonObjectMember = do
    (JsonString name) <- parseJsonString;
    spaces;
    char ':';
    spaces;
    value <- parseJsonValue;
    spaces;
    return (name, value);

parseJsonObject :: GenParser Char st JsonValue
parseJsonObject = do
    char '{';
    spaces;
    members <- (sepEndBy parseJsonObjectMember (try parseCommaSep));
    spaces;
    char '}';
    return (JsonObject (Map.fromList members));

parseJsonArray :: GenParser Char st JsonValue
parseJsonArray = do
    char '[';
    spaces;
    elements <- (sepEndBy parseJsonValue (try parseCommaSep));
    spaces;
    char ']';
    return (JsonArray elements);
    
parseJsonBoolean :: GenParser Char st JsonValue
parseJsonBoolean = do
    v <- string "true" <|> string "false"
    return (JsonBoolean (v == "true"))
    
parseJsonNull :: GenParser Char st JsonValue
parseJsonNull = do
    string "null"
    return JsonNull
    
    
parseJsonStringCharacters :: GenParser Char st String
parseJsonStringCharacters = do
    cs <- many ((noneOf "\"\\") <|> do { char '\\'; 
                                    c <- oneOf "\"\\/bfnrtu";
                                    case c of
                                        '\"' -> return '\"'
                                        '\\' -> return '\\'
                                        '/' -> return '/'
                                        'b' -> return '\b'
                                        'f' -> return '\f'
                                        'n' -> return '\n'
                                        't' -> return '\t'
                                        'u' -> do
                                            h1 <- hexDigit
                                            h2 <- hexDigit 
                                            h3 <- hexDigit 
                                            h4 <- hexDigit
                                            let val = ((digitToInt h1) * 4096) + 
                                                    ((digitToInt h2) * 256) + 
                                                    ((digitToInt h3) * 16) + 
                                                    (digitToInt h4)
                                            return (chr val)
                                            })    
    return cs

parseJsonString :: GenParser Char st JsonValue
parseJsonString = do
    char '"'
    jsonChars <- parseJsonStringCharacters
    char '"'
    return (JsonString jsonChars)
    
parseInteger :: [Char] -> Double
parseInteger [] = 0
parseInteger (c:cs) = (fromIntegral(digitToInt c) * (10 ** fromIntegral((length cs)))) + parseInteger cs
    
parseFractional :: [Char] -> Double
parseFractional [] = 0
parseFractional (c:cs) =  (fromIntegral(digitToInt c) / (10.0 ** fromIntegral((length cs) - 1))) + parseFractional cs

parseSign :: Maybe Char -> Double
parseSign (Just '-') = -1 
parseSign value = 1

parseIntegerPart :: GenParser Char st Double
parseIntegerPart = do
    sign <- optionMaybe  (char '-')
    digits <- many1 digit
    let signModifier = parseSign sign;
        integerPart = (parseInteger digits);
        result = signModifier * integerPart
    return (result)

parseExponentPart :: GenParser Char st Double
parseExponentPart = do
    oneOf "Ee"
    sign <- optionMaybe (oneOf "+-")
    digits <- many1 digit 
    
    let signModifier = parseSign sign;
        integerPart = (parseInteger digits);
        power = signModifier * integerPart;
        result = 10.0 ** power
    return result
    
parseFractionalPart :: GenParser Char st Double
parseFractionalPart = do
    char '.'
    digits <- many1 digit
    let n = parseInteger digits;
        pow = -(fromIntegral (length digits));
        result = n * (10.0 ** pow)
    return result 

parseJsonNumber :: GenParser Char st JsonValue
parseJsonNumber = do
    ip <- parseIntegerPart  
    fp <- parseFractionalPart <|> do { return 0.0; }    
    ep <- parseExponentPart <|> do { return 1.0; }
    return (JsonNumber ((ip + fp) * ep))
        
    
