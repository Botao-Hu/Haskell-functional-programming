--
-- S-expression parser.
--

module Sexpr where

import Text.Parsec
import Text.Parsec.String

----------------------------------------------------------------------
-- Datatypes.
----------------------------------------------------------------------

data Atom =
    BoolA  Bool
  | IntA   Integer
  | FloatA Double
  | IdA    String  -- identifier
  | StringA String
  deriving (Show)

data Sexpr =
    AtomS Atom
  | ListS [Sexpr]
  deriving (Show)

----------------------------------------------------------------------
-- Parsers.
----------------------------------------------------------------------

parseBool :: Parser Bool
parseBool =
  char '#' >>
  ((char 'f' >> return False)
   <|> (char 't' >> return True))
  <?> "boolean"

parseInt :: Parser Integer
parseInt = do
  sign <- option "" (string "-")
  digits <- many1 digit  -- many1 (oneOf "0123456789")
  return (read (sign ++ digits) :: Integer)
  <?> "integer"

parseFloat :: Parser Double
parseFloat = do
  sign <- option "" (string "-")
  digits <- many1 digit
  char '.'
  f <- many1 digit
  expo <- parseExpo
  return (read (sign ++ digits ++ "." ++ f ++ expo) :: Double)
  <?> "floating-point number"

-- A helper function to read in the exponential part of a float
parseExpo :: Parser String
parseExpo = do
  letter <- option "" (choice [string "e", string "E"])
  if letter == ""
    then return ""
    else do sign <- option "" (choice [string "+", string "-"])
            digits <- many1 digit
            return (letter ++ sign ++ digits)
            <?> "proper exponent format"
  
parseId :: Parser String
parseId = many1 (alphaNum <|> oneOf "_+-*/=?!") <?> "identifier"

-- String parser
parseString :: Parser String
parseString = do
  char '\"'
  string <- many (noneOf "\"")
  char '\"'
  return string
  <?> "string"

parseAtom :: Parser Atom
parseAtom =
  (parseBool >>= return . BoolA)
  <|> try (parseFloat >>= return . FloatA)
  <|> try (parseInt >>= return . IntA)
  <|> (parseString >>= return . StringA)
  <|> (parseId >>= return . IdA)
  <?> "atom"

parseComment :: Parser ()
parseComment = do
  char ';'
  many (noneOf "\n")
  char '\n'
  return ()

parseWhitespace :: Parser ()
parseWhitespace = many1 space >> return ()

-- Parse a separator (whitespace or comment).
parseSep :: Parser ()
parseSep = 
  many1 (parseComment <|> parseWhitespace) >> return ()
  <?> "separator"

-- Helper function to generate List parser which takes in different
-- delimiters.
parseListgen :: Char -> Char -> Parser [Sexpr]
parseListgen d1 d2 = do
  char d1
  optional parseSep
  ss <- parseSexpr `sepEndBy` parseSep
  char d2
  return ss
  <?> "list of S-expressions"

-- Parse a list of S-expressions, delimited by parentheses,
-- square brackets or curly braces, separated by whitespace/comments.
parseList :: Parser [Sexpr]
parseList = 
  (parseListgen '(' ')')
  <|> (parseListgen '[' ']')
  <|> (parseListgen '{' '}')
{-
Answer for Problem 3:
That is because if one of the parser fails, say, list parser for
parentheses delimiter fails for square brackets input, then this
parser WILL NOT CONSUME ANY INPUT since the function will break
at the first line (line 107) when it tries to parse certain
delimiter of its type. As a result, the whole input flow would go to
the next list parser without any loss.
-}

-- Parse a quoted expression.
parseQuote :: Parser Sexpr
parseQuote = char '\'' >> parseSexpr
  <?> "quoted S-expression"

-- Parse a single S-expressions.
parseSexpr :: Parser Sexpr
parseSexpr = 
  (parseAtom >>= return . AtomS)
  <|> (parseList >>= return . ListS)
  <|> (parseQuote >>= 
       return . (\x -> ListS [AtomS (IdA "quote"), x]))
  <?> "S-expression"

-- Parse a series of Sexprs from a string representing the entire 
-- contents of a file.
parseSexprsFromFile :: Parser [Sexpr]
parseSexprsFromFile = do
  optional parseSep
  ss <- parseSexpr `sepEndBy` parseSep
  eof
  return ss
  <?> "file of S-expressions"

----------------------------------------------------------------------
-- Pretty-printer.
----------------------------------------------------------------------

indent :: Int -> String
indent i = replicate i ' '

-- Pretty-print a Sexpr.
ppSexpr :: Int -> Sexpr -> String
ppSexpr i (AtomS a)  = indent i ++ show a
ppSexpr i (ListS ss) = 
  indent i
  ++ "ListS[\n" 
  ++ concatMap (\s -> ppSexpr (i + 2) s ++ "\n") ss
  ++ indent i ++ "]"

-- Parse all expressions in a file and run the pretty-printer on them.
runPpSexpr :: FilePath -> IO ()
runPpSexpr f = do
  p <- parseFromFile parseSexprsFromFile f
  case p of
    Left err -> putStrLn $ "ERROR: " ++ show err
    Right ss -> 
      mapM_ (\s -> do
        putStrLn (ppSexpr 0 s)
        putStrLn "") ss

----------------------------------------------------------------------
-- Tests.
----------------------------------------------------------------------

test :: IO ()
test = runPpSexpr "test.scm"

